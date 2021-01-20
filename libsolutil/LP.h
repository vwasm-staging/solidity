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
#pragma once

#include <libsmtutil/SolverInterface.h>

#include <libsolutil/Common.h>

#include <boost/rational.hpp>

#include <vector>
#include <variant>
#include <stack>

namespace solidity::util
{

struct Constraint
{
	std::vector<boost::rational<bigint>> data;
	bool equality = false;
};

struct State
{
	std::map<std::string, size_t> variables;
	std::vector<Constraint> constraints;
	/// x >= lowerBounds[x]
	std::map<size_t, boost::rational<bigint>> lowerBounds;
	/// x <= upperBounds[x]
	std::map<size_t, boost::rational<bigint>> upperBounds;
};


class LPSolver: public smtutil::SolverInterface
{
public:
	void reset();
	void push();
	void pop();

	void declareVariable(std::string const& _name, smtutil::SortPointer const& _sort) override;

	void addAssertion(smtutil::Expression const& _expr) override;

	std::pair<smtutil::CheckResult, std::vector<std::string>>
	check(std::vector<smtutil::Expression> const& _expressionsToEvaluate) override;

private:
	using rational = boost::rational<bigint>;

	/// Parses the expression and expects a linear sum of variables.
	/// Returns a vector with the first element being the constant and the
	/// other elements the factors for the respective variables.
	/// If the expression cannot be properly parsed or is not linear,
	/// returns an empty vector.
	std::optional<std::vector<rational>> parseLinearSum(smtutil::Expression const& _expression) const;
	std::optional<std::vector<rational>> parseProduct(smtutil::Expression const& _expression) const;
	std::optional<std::vector<rational>> parseFactor(smtutil::Expression const& _expression) const;

	std::string variableName(size_t _index) const;

	std::stack<State> m_state{{State{}}};
};


}
