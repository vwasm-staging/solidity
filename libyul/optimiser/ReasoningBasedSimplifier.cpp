/*
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

#include <libyul/optimiser/ReasoningBasedSimplifier.h>

#include <libyul/optimiser/SSAValueTracker.h>
#include <libyul/optimiser/Semantics.h>
#include <libyul/AST.h>
#include <libyul/Utilities.h>
#include <libyul/Dialect.h>

#include <libyul/backends/evm/EVMDialect.h>

#include <libsmtutil/SMTPortfolio.h>
#include <libsmtutil/Helpers.h>

#include <libsolutil/Visitor.h>
#include <libsolutil/CommonData.h>
#include <libsolutil/LP.h>

#include <utility>
#include <memory>

using namespace std;
using namespace solidity;
using namespace solidity::util;
using namespace solidity::yul;
using namespace solidity::smtutil;

void ReasoningBasedSimplifier::run(OptimiserStepContext& _context, Block& _ast)
{
	set<YulString> ssaVars = SSAValueTracker::ssaVariables(_ast);
	ReasoningBasedSimplifier{_context.dialect, ssaVars}(_ast);
}

std::optional<string> ReasoningBasedSimplifier::invalidInCurrentEnvironment()
{
//	// SMTLib2 interface is always available, but we would like to have synchronous answers.
//	if (smtutil::SMTPortfolio{}.solvers() <= 1)
//		return string{"No SMT solvers available."};
//	else
		return nullopt;
}

void ReasoningBasedSimplifier::operator()(VariableDeclaration& _varDecl)
{
	if (_varDecl.variables.size() != 1 || !_varDecl.value)
		return;
	YulString varName = _varDecl.variables.front().name;
	if (!m_ssaVariables.count(varName))
		return;
	// TODO should be boolean depending on th efunction
	smtutil::Expression variable = newRestrictedVariable("yul_" + varName.str());
	bool const inserted = m_variables.insert({varName, variable}).second;
	yulAssert(inserted, "");
	if (!_varDecl.value)
		return; // TODO we could encode zero, but the variable should not be used anyway.

	std::visit(GenericVisitor{
		[&](FunctionCall const& _functionCall)
		{
			if (auto const* dialect = dynamic_cast<EVMDialect const*>(&m_dialect))
				if (auto const* builtin = dialect->builtin(_functionCall.functionName.name))
					if (builtin->instruction)
							handleDeclaration(varName, *builtin->instruction, _functionCall.arguments);
		},
		[&](Identifier const& _identifier)
		{
			if (
				m_ssaVariables.count(_identifier.name) &&
				m_variables.count(_identifier.name)
			)
				m_solver->addAssertion(variable == m_variables.at(_identifier.name));
		},
		[&](Literal const& _literal)
		{
			m_solver->addAssertion(variable == literalValue(_literal));
		}
	}, *_varDecl.value);
}

void ReasoningBasedSimplifier::operator()(If& _if)
{
	if (!SideEffectsCollector{m_dialect, *_if.condition}.movable())
		return;

	if (!holds_alternative<Identifier>(*_if.condition))
	{
		ASTModifier::operator()(_if.body);
		return;
	}
	Identifier const& condition = get<Identifier>(*_if.condition);
	if (!m_ssaVariables.count(condition.name) || !m_variables.count(condition.name))
	{
		ASTModifier::operator()(_if.body);
		return;
	}
	smtutil::Expression cond = m_variables.at(condition.name);

	m_solver->push();
	restrictEqualZero(cond);
	bool constantTrue = infeasible();
	m_solver->pop();

	if (constantTrue)
	{
		Literal trueCondition = m_dialect.trueLiteral();
		trueCondition.location = locationOf(*_if.condition);
		_if.condition = make_unique<yul::Expression>(move(trueCondition));
	}
	else
	{
		m_solver->push();
		restrictNonzero(cond);
		bool constantFalse = infeasible();
		m_solver->pop();

		if (constantFalse)
		{
			Literal falseCondition = m_dialect.zeroLiteralForType(m_dialect.boolType);
			falseCondition.location = locationOf(*_if.condition);
			_if.condition = make_unique<yul::Expression>(move(falseCondition));
			_if.body = yul::Block{};
			// Nothing left to be done.
			return;
		}
	}

	m_solver->push();
	restrictNonzero(cond);

	ASTModifier::operator()(_if.body);

	m_solver->pop();
}

ReasoningBasedSimplifier::ReasoningBasedSimplifier(
	Dialect const& _dialect,
	set<YulString> const& _ssaVariables
):
	m_dialect(_dialect),
	m_ssaVariables(_ssaVariables),
	m_solver(make_unique<util::BooleanLPSolver>())
{
}

void ReasoningBasedSimplifier::handleDeclaration(
	YulString _varName,
	evmasm::Instruction _instruction,
	vector<yul::Expression> const& _arguments
)
{
	smtutil::Expression variable = m_variables.at(_varName);
	vector<smtutil::Expression> arguments;
	for (yul::Expression const& arg: _arguments)
	{
		// TODO this disallows literals as function arguments.
		if (!holds_alternative<Identifier>(arg))
			return;
		Identifier const& v = get<Identifier>(arg);
		if (!m_ssaVariables.count(v.name) || !m_variables.count(v.name))
			return;
		arguments.push_back(m_variables.at(v.name));
	}

	optional<smtutil::Expression> x;
	optional<smtutil::Expression> y;
	optional<smtutil::Expression> z;
	if (arguments.size() > 0)
		x = arguments.at(0);
	if (arguments.size() > 1)
		y = arguments.at(1);
	if (arguments.size() > 2)
		z = arguments.at(2);

	switch (_instruction)
	{
	case evmasm::Instruction::ADD:
		if (makesInfeasible(*x + *y >= smtutil::Expression(bigint(1) << 256)))
			m_solver->addAssertion(variable == *x + *y);
		break;
	case evmasm::Instruction::SUB:
		if (makesInfeasible(*x - *y < 0))
			m_solver->addAssertion(variable == *x - *y);
		break;
	//case evmasm::Instruction::MUL:
		// TODO encode constants?
	//case evmasm::Instruction::DIV:
	case evmasm::Instruction::ADDMOD:
		m_solver->addAssertion(variable < *z);
		break;
	case evmasm::Instruction::LT:
		tryAddBoolean(_varName, *x < *y, {*x >= *y});
		break;
	case evmasm::Instruction::GT:
		tryAddBoolean(_varName, *x > *y, {*x <= *y});
		break;
	case evmasm::Instruction::EQ:
		tryAddBoolean(_varName, *x == *y, {*x < *y, *x > *y});
		break;
	case evmasm::Instruction::ISZERO:
		tryAddBoolean(_varName, !*x, {*x});
		break;
	case evmasm::Instruction::AND:
		m_solver->addAssertion(variable <= *x && variable <= *y);
		m_nonEncodedValues.insert({_varName, *x && *y});
		break;
	case evmasm::Instruction::OR:
		m_solver->addAssertion(variable >= *x && variable >= *y);
		m_solver->addAssertion(variable <= *x + *y);
		m_nonEncodedValues.insert({_varName, *x || *y});
		break;
	// TODO all builtins whose return values can be restricted.
	default:
		break;
	}
}

smtutil::Expression ReasoningBasedSimplifier::newVariable(string const& _name)
{
	return m_solver->newVariable(_name.empty() ? uniqueName() : _name, defaultSort());
}

smtutil::Expression ReasoningBasedSimplifier::newRestrictedVariable(string const& _name)
{
	smtutil::Expression var = newVariable(_name);
	m_solver->addAssertion(var < smtutil::Expression(bigint(1) << 256));
	return var;
}

string ReasoningBasedSimplifier::uniqueName()
{
	return "expr_" + to_string(m_varCounter++);
}

bool ReasoningBasedSimplifier::makesInfeasible(smtutil::Expression _constraint)
{
	m_solver->push();
	m_solver->addAssertion(_constraint);
	bool result = infeasible();
	m_solver->pop();
	return result;
}

bool ReasoningBasedSimplifier::feasible()
{
	CheckResult result = m_solver->check({}).first;
	return result == CheckResult::SATISFIABLE;
}

bool ReasoningBasedSimplifier::infeasible()
{
	CheckResult result = m_solver->check({}).first;
	return result == CheckResult::UNSATISFIABLE;
}

void ReasoningBasedSimplifier::tryAddBoolean(
	YulString _varName,
	smtutil::Expression _value,
	vector<smtutil::Expression> const& _negations
)
{
	smtutil::Expression variable = m_variables.at(_varName);

	if (makesInfeasible(_value))
	{
		m_solver->addAssertion(variable == 0);
		if (_negations.size() == 1)
			m_solver->addAssertion(_negations.front());
		return;
	}
	for (smtutil::Expression const& negation: _negations)
		if (makesInfeasible(negation))
		{
			m_solver->addAssertion(variable == 1);
			m_solver->addAssertion(_value);
			return;
		}

	m_solver->addAssertion(variable <= 1);
	m_nonEncodedValues.insert({_varName, _value});
}

YulString ReasoningBasedSimplifier::localVariableFromExpression(string const& _expressionName)
{
	solAssert(_expressionName.substr(0, 4) == "yul_", "");
	return YulString(_expressionName.substr(4));

}

void ReasoningBasedSimplifier::restrictEqualZero(smtutil::Expression _constraint)
{
	m_solver->addAssertion(_constraint == constantValue(0));

	if (_constraint.arguments.empty())
	{
		// TODO the whole solver has to be change so that it can either return
		// "infeasible" or "unknown".
		// This means we can always drop constraintse, and we can move these to restriction
		// functions into the solver itself.
		YulString localVar = localVariableFromExpression(_constraint.name);
		if (m_nonEncodedValues.count(localVar))
			restrictEqualZero(m_nonEncodedValues.at(localVar));
	}
	else if (_constraint.name == "or")
	{
		restrictEqualZero(_constraint.arguments.at(0));
		restrictEqualZero(_constraint.arguments.at(1));
	}
	else if (_constraint.name == "not")
		restrictNonzero(_constraint.arguments.at(0));
	else if (_constraint.name == ">=")
		m_solver->addAssertion(_constraint.arguments.at(0) < _constraint.arguments.at(1));
	else if (_constraint.name == "<=")
		m_solver->addAssertion(_constraint.arguments.at(0) > _constraint.arguments.at(1));
	else if (_constraint.name == ">")
		m_solver->addAssertion(_constraint.arguments.at(0) <= _constraint.arguments.at(1));
	else if (_constraint.name == "<")
		m_solver->addAssertion(_constraint.arguments.at(0) >= _constraint.arguments.at(1));
}

void ReasoningBasedSimplifier::restrictNonzero(smtutil::Expression _constraint)
{
	m_solver->addAssertion(_constraint >= constantValue(1));

	if (_constraint.arguments.empty())
	{
		// TODO the whole solver has to be change so that it can either return
		// "infeasible" or "unknown".
		// This means we can always drop constraintse, and we can move these to restriction
		// functions into the solver itself.
		YulString localVar = localVariableFromExpression(_constraint.name);
		if (m_nonEncodedValues.count(localVar))
			restrictNonzero(m_nonEncodedValues.at(localVar));
	}
	else if (_constraint.name == "and")
	{
		restrictNonzero(_constraint.arguments.at(0));
		restrictNonzero(_constraint.arguments.at(1));
	}
	else if (_constraint.name == "not")
		restrictEqualZero(_constraint.arguments.at(0));
	else if (_constraint.name == ">=")
		m_solver->addAssertion(_constraint.arguments.at(0) >= _constraint.arguments.at(1));
	else if (_constraint.name == "<=")
		m_solver->addAssertion(_constraint.arguments.at(0) <= _constraint.arguments.at(1));
	else if (_constraint.name == ">")
		m_solver->addAssertion(_constraint.arguments.at(0) > _constraint.arguments.at(1));
	else if (_constraint.name == "<")
		m_solver->addAssertion(_constraint.arguments.at(0) < _constraint.arguments.at(1));
	else if (_constraint.name == "=")
		m_solver->addAssertion(_constraint.arguments.at(0) == _constraint.arguments.at(1));
}

shared_ptr<Sort> ReasoningBasedSimplifier::defaultSort() const
{
	return SortProvider::intSort();
}

smtutil::Expression ReasoningBasedSimplifier::booleanValue(smtutil::Expression _value) const
{
	return smtutil::Expression::ite(_value, constantValue(1), constantValue(0));
}

smtutil::Expression ReasoningBasedSimplifier::constantValue(size_t _value) const
{
	return _value;
}

smtutil::Expression ReasoningBasedSimplifier::literalValue(Literal const& _literal) const
{
	return smtutil::Expression(valueOfLiteral(_literal));
}
