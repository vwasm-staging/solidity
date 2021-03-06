/*(
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * Base class to perform data flow analysis during AST walks.
 * Tracks assignments and is used as base class for both Rematerialiser and
 * Common Subexpression Eliminator.
 */

#include <libyul/optimiser/DataFlowAnalyzer.h>

#include <libyul/optimiser/NameCollector.h>
#include <libyul/optimiser/Semantics.h>
#include <libyul/AST.h>
#include <libyul/Dialect.h>
#include <libyul/Exceptions.h>

#include <libsolutil/CommonData.h>

#include <variant>

using namespace std;
using namespace solidity;
using namespace solidity::util;
using namespace solidity::yul;

DataFlowAnalyzer::DataFlowAnalyzer(
	Dialect const& _dialect,
	map<YulString, SideEffects> _functionSideEffects
):
m_dialect(_dialect),
m_functionSideEffects(std::move(_functionSideEffects)),
m_knowledgeBase(_dialect, m_value)
{
	if (auto const* builtin = _dialect.memoryStoreFunction(YulString{}))
		m_storeFunctionName[static_cast<unsigned>(StoreLoadLocation::Memory)] = builtin->name;
	if (auto const* builtin = _dialect.memoryLoadFunction(YulString{}))
		m_loadFunctionName[static_cast<unsigned>(StoreLoadLocation::Memory)] = builtin->name;
	if (auto const* builtin = _dialect.storageStoreFunction(YulString{}))
		m_storeFunctionName[static_cast<unsigned>(StoreLoadLocation::Storage)] = builtin->name;
	if (auto const* builtin = _dialect.storageLoadFunction(YulString{}))
		m_loadFunctionName[static_cast<unsigned>(StoreLoadLocation::Storage)] = builtin->name;
}

void DataFlowAnalyzer::operator()(ExpressionStatement& _statement)
{
	if (auto vars = isSimpleStore(StoreLoadLocation::Storage, _statement))
	{
		ASTModifier::operator()(_statement);
		set<YulString> keysToErase;
		for (auto const& item: m_storage.values)
			if (!(
				m_knowledgeBase.knownToBeDifferent(vars->first, item.first) ||
				m_knowledgeBase.knownToBeEqual(vars->second, item.second)
			))
				keysToErase.insert(item.first);
		for (YulString const& key: keysToErase)
			m_storage.eraseKey(key);
		m_storage.set(vars->first, vars->second);
	}
	else if (auto vars = isSimpleStore(StoreLoadLocation::Memory, _statement))
	{
		ASTModifier::operator()(_statement);
		set<YulString> keysToErase;
		for (auto const& item: m_memory.values)
			if (!m_knowledgeBase.knownToBeDifferentByAtLeast32(vars->first, item.first))
				keysToErase.insert(item.first);
		for (YulString const& key: keysToErase)
			m_memory.eraseKey(key);
		m_memory.set(vars->first, vars->second);
	}
	else
	{
		clearKnowledgeIfInvalidated(_statement.expression);
		ASTModifier::operator()(_statement);
	}
}

void DataFlowAnalyzer::operator()(Assignment& _assignment)
{
	set<YulString> names;
	for (auto const& var: _assignment.variableNames)
		names.emplace(var.name);
	assertThrow(_assignment.value, OptimizerException, "");
	clearKnowledgeIfInvalidated(*_assignment.value);
	visit(*_assignment.value);
	handleAssignment(names, _assignment.value.get(), false);
}

void DataFlowAnalyzer::operator()(VariableDeclaration& _varDecl)
{
	if (_varDecl.value)
		clearKnowledgeIfInvalidated(*_varDecl.value);

	Scoper::operator()(_varDecl);

	set<YulString> names;
	for (auto const& var: _varDecl.variables)
		names.emplace(var.name);

	handleAssignment(names, _varDecl.value.get(), true);
}

void DataFlowAnalyzer::operator()(If& _if)
{
	clearKnowledgeIfInvalidated(*_if.condition);
	InvertibleMap<YulString, YulString> storage = m_storage;
	InvertibleMap<YulString, YulString> memory = m_memory;

	ASTModifier::operator()(_if);

	joinKnowledge(storage, memory);

	Assignments assignments;
	assignments(_if.body);
	clearValues(assignments.names());
}

void DataFlowAnalyzer::operator()(Switch& _switch)
{
	clearKnowledgeIfInvalidated(*_switch.expression);
	visit(*_switch.expression);
	set<YulString> assignedVariables;
	for (auto& _case: _switch.cases)
	{
		InvertibleMap<YulString, YulString> storage = m_storage;
		InvertibleMap<YulString, YulString> memory = m_memory;
		(*this)(_case.body);
		joinKnowledge(storage, memory);

		Assignments assignments;
		assignments(_case.body);
		assignedVariables += assignments.names();
		// This is a little too destructive, we could retain the old values.
		clearValues(assignments.names());
		clearKnowledgeIfInvalidated(_case.body);
	}
	for (auto& _case: _switch.cases)
		clearKnowledgeIfInvalidated(_case.body);
	clearValues(assignedVariables);
}

void DataFlowAnalyzer::operator()(FunctionDefinition& _fun)
{
	// Save all information. We might rather reinstantiate this class,
	// but this could be difficult if it is subclassed.
	map<YulString, AssignedValue> value;
	size_t loopDepth{0};
	InvertibleRelation<YulString> references;
	InvertibleMap<YulString, YulString> storage;
	InvertibleMap<YulString, YulString> memory;
	swap(m_value, value);
	swap(m_loopDepth, loopDepth);
	swap(m_references, references);
	swap(m_storage, storage);
	swap(m_memory, memory);

	for (auto const& var: _fun.returnVariables)
		handleAssignment({var.name}, nullptr, true);

	Scoper::operator()(_fun);

	// Note that the contents of return variables, storage and memory at this point
	// might be incorrect due to the fact that the DataFlowAnalyzer ignores the ``leave``
	// statement.
	swap(m_value, value);
	swap(m_loopDepth, loopDepth);
	swap(m_references, references);
	swap(m_storage, storage);
	swap(m_memory, memory);
}

void DataFlowAnalyzer::operator()(ForLoop& _for)
{
	// If the pre block was not empty,
	// we would have to deal with more complicated scoping rules.
	assertThrow(_for.pre.statements.empty(), OptimizerException, "");

	++m_loopDepth;

	AssignmentsSinceContinue assignmentsSinceCont;
	assignmentsSinceCont(_for.body);

	Assignments assignments;
	assignments(_for.body);
	assignments(_for.post);
	clearValues(assignments.names());

	// break/continue are tricky for storage and thus we almost always clear here.
	clearKnowledgeIfInvalidated(*_for.condition);
	clearKnowledgeIfInvalidated(_for.post);
	clearKnowledgeIfInvalidated(_for.body);

	visit(*_for.condition);
	(*this)(_for.body);
	clearValues(assignmentsSinceCont.names());
	clearKnowledgeIfInvalidated(_for.body);
	(*this)(_for.post);
	clearValues(assignments.names());
	clearKnowledgeIfInvalidated(*_for.condition);
	clearKnowledgeIfInvalidated(_for.post);
	clearKnowledgeIfInvalidated(_for.body);

	--m_loopDepth;
}


void DataFlowAnalyzer::handleAssignment(set<YulString> const& _variables, Expression* _value, bool _isDeclaration)
{
	if (!_isDeclaration)
		clearValues(_variables);

	MovableChecker movableChecker{m_dialect, &m_functionSideEffects};
	if (_value)
		movableChecker.visit(*_value);
	else
		for (auto const& var: _variables)
			assignValue(var, &m_zero);

	if (_value && _variables.size() == 1)
	{
		YulString name = *_variables.begin();
		// Expression has to be movable and cannot contain a reference
		// to the variable that will be assigned to.
		if (movableChecker.movable() && !movableChecker.referencedVariables().count(name))
			assignValue(name, _value);
	}

	auto const& referencedVariables = movableChecker.referencedVariables();
	for (auto const& name: _variables)
	{
		m_references.set(name, referencedVariables);
		if (!_isDeclaration)
		{
			// assignment to slot denoted by "name"
			m_storage.eraseKey(name);
			// assignment to slot contents denoted by "name"
			m_storage.eraseValue(name);
			// assignment to slot denoted by "name"
			m_memory.eraseKey(name);
			// assignment to slot contents denoted by "name"
			m_memory.eraseValue(name);
		}
	}

	if (_value && _variables.size() == 1)
	{
		YulString variable = *_variables.begin();
		if (!movableChecker.referencedVariables().count(variable))
		{
			// This might erase additional knowledge about the slot.
			// On the other hand, if we knew the value in the slot
			// already, then the sload() / mload() would have been replaced by a variable anyway.
			if (auto key = isSimpleLoad(StoreLoadLocation::Memory, *_value))
				m_memory.set(*key, variable);
			else if (auto key = isSimpleLoad(StoreLoadLocation::Storage, *_value))
				m_storage.set(*key, variable);
		}
	}
}

void DataFlowAnalyzer::popScope()
{
	clearValues(std::move(m_variableScopes.back().variables));
	m_variableScopes.pop_back();
}

void DataFlowAnalyzer::clearValues(set<YulString> _variables)
{
	// All variables that reference variables to be cleared also have to be
	// cleared, but not recursively, since only the value of the original
	// variables changes. Example:
	// let a := 1
	// let b := a
	// let c := b
	// let a := 2
	// add(b, c)
	// In the last line, we can replace c by b, but not b by a.
	//
	// This cannot be easily tested since the substitutions will be done
	// one by one on the fly, and the last line will just be add(1, 1)

	// First clear storage knowledge, because we do not have to clear
	// storage knowledge of variables whose expression has changed,
	// since the value is still unchanged.
	for (auto const& name: _variables)
	{
		// clear slot denoted by "name"
		m_storage.eraseKey(name);
		// clear slot contents denoted by "name"
		m_storage.eraseValue(name);
		// assignment to slot denoted by "name"
		m_memory.eraseKey(name);
		// assignment to slot contents denoted by "name"
		m_memory.eraseValue(name);
	}

	// Also clear variables that reference variables to be cleared.
	for (auto const& name: _variables)
		for (auto const& ref: m_references.backward[name])
			_variables.emplace(ref);

	// Clear the value and update the reference relation.
	for (auto const& name: _variables)
		m_value.erase(name);
	for (auto const& name: _variables)
		m_references.eraseKey(name);
}

void DataFlowAnalyzer::assignValue(YulString _variable, Expression const* _value)
{
	m_value[_variable] = {_value, m_loopDepth};
}

void DataFlowAnalyzer::clearKnowledgeIfInvalidated(Block const& _block)
{
	SideEffectsCollector sideEffects(m_dialect, _block, &m_functionSideEffects);
	if (sideEffects.invalidatesStorage())
		m_storage.clear();
	if (sideEffects.invalidatesMemory())
		m_memory.clear();
}

void DataFlowAnalyzer::clearKnowledgeIfInvalidated(Expression const& _expr)
{
	SideEffectsCollector sideEffects(m_dialect, _expr, &m_functionSideEffects);
	if (sideEffects.invalidatesStorage())
		m_storage.clear();
	if (sideEffects.invalidatesMemory())
		m_memory.clear();
}

void DataFlowAnalyzer::joinKnowledge(
	InvertibleMap<YulString, YulString> const& _olderStorage,
	InvertibleMap<YulString, YulString> const& _olderMemory
)
{
	joinKnowledgeHelper(m_storage, _olderStorage);
	joinKnowledgeHelper(m_memory, _olderMemory);
}

void DataFlowAnalyzer::joinKnowledgeHelper(
	InvertibleMap<YulString, YulString>& _this,
	InvertibleMap<YulString, YulString> const& _older
)
{
	// We clear if the key does not exist in the older map or if the value is different.
	// This also works for memory because _older is an "older version"
	// of m_memory and thus any overlapping write would have cleared the keys
	// that are not known to be different inside m_memory already.
	set<YulString> keysToErase;
	for (auto const& item: _this.values)
	{
		auto it = _older.values.find(item.first);
		if (it == _older.values.end() || it->second != item.second)
			keysToErase.insert(item.first);
	}
	for (auto const& key: keysToErase)
		_this.eraseKey(key);
}

std::optional<pair<YulString, YulString>> DataFlowAnalyzer::isSimpleStore(
	StoreLoadLocation _location,
	ExpressionStatement const& _statement
) const
{
	if (FunctionCall const* funCall = get_if<FunctionCall>(&_statement.expression))
		if (funCall->functionName.name == m_storeFunctionName[static_cast<unsigned>(_location)])
			if (Identifier const* key = std::get_if<Identifier>(&funCall->arguments.front()))
				if (Identifier const* value = std::get_if<Identifier>(&funCall->arguments.back()))
					return make_pair(key->name, value->name);
	return {};
}

std::optional<YulString> DataFlowAnalyzer::isSimpleLoad(
	StoreLoadLocation _location,
	Expression const& _expression
) const
{
	if (FunctionCall const* funCall = get_if<FunctionCall>(&_expression))
		if (funCall->functionName.name == m_loadFunctionName[static_cast<unsigned>(_location)])
			if (Identifier const* key = std::get_if<Identifier>(&funCall->arguments.front()))
				return key->name;
	return {};
}
