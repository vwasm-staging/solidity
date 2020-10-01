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
// SPDX-License-Identifier: GPL-3.0

#include <libsolidity/formal/SymbolicState.h>

#include <libsolidity/formal/SymbolicTypes.h>
#include <libsolidity/formal/EncodingContext.h>

using namespace std;
using namespace solidity;
using namespace solidity::smtutil;
using namespace solidity::frontend::smt;

SymbolicState::SymbolicState(EncodingContext& _context):
	m_context(_context)
{
	/// Build state tuple.
	m_stateMembers.emplace("balances", make_shared<smtutil::ArraySort>(smtutil::SortProvider::uintSort, smtutil::SortProvider::uintSort));

	unsigned i = 0;
	vector<string> members;
	vector<SortPointer> sorts;
	for (auto const& [component, sort]: m_stateMembers)
	{
		members.emplace_back(component);
		sorts.emplace_back(sort);
		m_stateComponentIndices[component] = i++;
	}
	m_stateTuple = make_unique<SymbolicTupleVariable>(
		make_shared<smtutil::TupleSort>("state_type", members, sorts),
		"state",
		m_context
	);

	/// Build tx tuple.
	m_txMembers.emplace("blockhash", make_shared<smtutil::ArraySort>(smtutil::SortProvider::uintSort, smtutil::SortProvider::uintSort));
	m_txMembers.emplace("block.coinbase", smt::smtSort(*TypeProvider::address()));
	m_txMembers.emplace("block.difficulty", smtutil::SortProvider::uintSort);
	m_txMembers.emplace("block.gaslimit", smtutil::SortProvider::uintSort);
	m_txMembers.emplace("block.number", smtutil::SortProvider::uintSort);
	m_txMembers.emplace("block.timestamp", smtutil::SortProvider::uintSort);
	// TODO gasleft
	m_txMembers.emplace("msg.data", smt::smtSort(*TypeProvider::bytesMemory()));
	m_txMembers.emplace("msg.sender", smt::smtSort(*TypeProvider::address()));
	m_txMembers.emplace("msg.sig", smtutil::SortProvider::uintSort);
	m_txMembers.emplace("msg.value", smtutil::SortProvider::uintSort);
	m_txMembers.emplace("tx.gasprice", smtutil::SortProvider::uintSort);
	m_txMembers.emplace("tx.origin", smtutil::SortProvider::uintSort);

	i = 0;
	members.clear();
	sorts.clear();
	for (auto const& [component, sort]: m_txMembers)
	{
		members.emplace_back(component);
		sorts.emplace_back(sort);
		m_txComponentIndices[component] = i++;
	}
	m_txTuple = make_unique<SymbolicTupleVariable>(
		make_shared<smtutil::TupleSort>("tx_type", members, sorts),
		"tx",
		m_context
	);

	/// Build crypto functions tuple.
	SortPointer bytesSort = smtSort(*TypeProvider::bytesStorage());
	SortPointer bytes32Sort = smtSort(*TypeProvider::fixedBytes(32));
	m_cryptoMembers.emplace("keccak256", make_shared<smtutil::ArraySort>(bytesSort, bytes32Sort));
	m_cryptoMembers.emplace("sha256", make_shared<smtutil::ArraySort>(bytesSort, bytes32Sort));
	m_cryptoMembers.emplace("ripemd160", make_shared<smtutil::ArraySort>(bytesSort, smtSort(*TypeProvider::fixedBytes(20))));
	m_cryptoMembers.emplace("ripemd160", make_shared<smtutil::ArraySort>(bytesSort, smtSort(*TypeProvider::fixedBytes(20))));
	SortPointer ecrecoverInputSort = make_shared<TupleSort>(
		"ecrecover_input_type",
		vector<string>{"hash", "v", "r", "s"},
		vector<SortPointer>{bytes32Sort, smtSort(*TypeProvider::uint(8)), bytes32Sort, bytes32Sort}
	);
	m_cryptoMembers.emplace("ecrecover", make_shared<smtutil::ArraySort>(ecrecoverInputSort, smtSort(*TypeProvider::address())));

	i = 0;
	members.clear();
	sorts.clear();
	for (auto const& [component, sort]: m_cryptoMembers)
	{
		members.emplace_back(component);
		sorts.emplace_back(sort);
		m_cryptoComponentIndices[component] = i++;
	}
	m_cryptoTuple = make_unique<SymbolicTupleVariable>(
		make_shared<smtutil::TupleSort>("crypto_functions_type", members, sorts),
		"crypto_functions",
		m_context
	);
}

void SymbolicState::reset()
{
	m_error.resetIndex();
	m_thisAddress.resetIndex();
	m_stateTuple->resetIndex();
	m_txTuple->resetIndex();
}

// Blockchain

SymbolicIntVariable& SymbolicState::errorFlag()
{
	return m_error;
}

SortPointer SymbolicState::errorFlagSort()
{
	return m_error.sort();
}

smtutil::Expression SymbolicState::thisAddress()
{
	return m_thisAddress.currentValue();
}

smtutil::Expression SymbolicState::thisAddress(unsigned _idx)
{
	return m_thisAddress.valueAtIndex(_idx);
}

SortPointer SymbolicState::thisAddressSort()
{
	return m_thisAddress.sort();
}

smtutil::Expression SymbolicState::state()
{
	return m_stateTuple->currentValue();
}

smtutil::Expression SymbolicState::state(unsigned _idx)
{
	return m_stateTuple->valueAtIndex(_idx);
}

SortPointer SymbolicState::stateSort()
{
	return m_stateTuple->sort();
}

void SymbolicState::newState()
{
	m_stateTuple->increaseIndex();
}

smtutil::Expression SymbolicState::tx()
{
	return m_txTuple->currentValue();
}

smtutil::Expression SymbolicState::tx(unsigned _idx)
{
	return m_txTuple->valueAtIndex(_idx);
}

SortPointer SymbolicState::txSort()
{
	return m_txTuple->sort();
}

void SymbolicState::newTx()
{
	m_txTuple->increaseIndex();
}

void SymbolicState::addTxConstraints(FunctionDefinition const& _function)
{
	smt::setSymbolicUnknownValue(txMember("block.coinbase"), TypeProvider::uint(160), m_context);
	smt::setSymbolicUnknownValue(txMember("msg.sender"), TypeProvider::uint(160), m_context);
	smt::setSymbolicUnknownValue(txMember("tx.origin"), TypeProvider::uint(160), m_context);

	if (_function.isPartOfExternalInterface())
	{
		auto sig = TypeProvider::function(_function)->externalIdentifier();
		m_context.addAssertion(txMember("msg.sig") == sig);

		auto b0 = sig >> (3 * 8);
		auto b1 = (sig & 0x00ff0000) >> (2 * 8);
		auto b2 = (sig & 0x0000ff00) >> (1 * 8);
		auto b3 = (sig & 0x000000ff);
		auto data = smtutil::Expression::tuple_get(txMember("msg.data"), 0);
		m_context.addAssertion(smtutil::Expression::select(data, 0) == b0);
		m_context.addAssertion(smtutil::Expression::select(data, 1) == b1);
		m_context.addAssertion(smtutil::Expression::select(data, 2) == b2);
		m_context.addAssertion(smtutil::Expression::select(data, 3) == b3);
	}
}

smtutil::Expression SymbolicState::crypto()
{
	return m_cryptoTuple->currentValue();
}

smtutil::Expression SymbolicState::crypto(unsigned _idx)
{
	return m_cryptoTuple->valueAtIndex(_idx);
}

SortPointer SymbolicState::cryptoSort()
{
	return m_cryptoTuple->sort();
}

void SymbolicState::newCrypto()
{
	m_cryptoTuple->increaseIndex();
}

smtutil::Expression SymbolicState::balances()
{
	return m_stateTuple->component(m_stateComponentIndices.at("balances"));
}

smtutil::Expression SymbolicState::balance()
{
	return balance(thisAddress());
}

smtutil::Expression SymbolicState::balance(smtutil::Expression _address)
{
	return smtutil::Expression::select(balances(), move(_address));
}

smtutil::Expression SymbolicState::blockhash(smtutil::Expression _blockNumber)
{
	return smtutil::Expression::select(txMember("blockhash"), move(_blockNumber));
}

smtutil::Expression SymbolicState::txMember(string const& _member)
{
	return m_txTuple->component(m_txComponentIndices.at(_member));
}

smtutil::Expression SymbolicState::cryptoFunction(string const& _member)
{
	return m_cryptoTuple->component(m_cryptoComponentIndices.at(_member));
}

void SymbolicState::transfer(smtutil::Expression _from, smtutil::Expression _to, smtutil::Expression _value)
{
	unsigned indexBefore = m_stateTuple->index();
	addBalance(_from, 0 - _value);
	addBalance(_to, move(_value));
	unsigned indexAfter = m_stateTuple->index();
	solAssert(indexAfter > indexBefore, "");
	m_stateTuple->increaseIndex();
	/// Do not apply the transfer operation if _from == _to.
	auto newState = smtutil::Expression::ite(
		move(_from) == move(_to),
		m_stateTuple->valueAtIndex(indexBefore),
		m_stateTuple->valueAtIndex(indexAfter)
	);
	m_context.addAssertion(m_stateTuple->currentValue() == newState);
}

/// Private helpers.

void SymbolicState::addBalance(smtutil::Expression _address, smtutil::Expression _value)
{
	auto newBalances = smtutil::Expression::store(
		balances(),
		_address,
		balance(_address) + move(_value)
	);
	assignStateMember("balances", newBalances);
}

smtutil::Expression SymbolicState::assignStateMember(string const& _member, smtutil::Expression const& _value)
{
	vector<smtutil::Expression> args;
	for (auto const& member: m_stateMembers)
		if (member.first == _member)
			args.emplace_back(_value);
		else
			args.emplace_back(m_stateTuple->component(m_stateComponentIndices.at(member.first)));
	m_stateTuple->increaseIndex();
	auto tuple = m_stateTuple->currentValue();
	auto sortExpr = smtutil::Expression(make_shared<smtutil::SortSort>(tuple.sort), tuple.name);
	m_context.addAssertion(tuple == smtutil::Expression::tuple_constructor(sortExpr, args));
	return m_stateTuple->currentValue();
}
