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

#include <libsolutil/LP.h>

#include <libsolutil/CommonData.h>
#include <libsolutil/StringUtils.h>
#include <liblangutil/Exceptions.h>

#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/transform.hpp>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/tail.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/algorithm/all_of.hpp>
#include <range/v3/algorithm/max.hpp>
#include <range/v3/algorithm/count_if.hpp>

using namespace std;
using namespace solidity;
using namespace solidity::util;
using namespace solidity::smtutil;

using rational = boost::rational<bigint>;


namespace
{

string toString(rational const& _value, size_t _paddedLength = 2)
{
	string result;
	if (_value == rational(bigint(1) << 256))
		result = "2**256";
	else if (_value == rational(bigint(1) << 256) - 1)
		result = "2**256-1";
	else if (_value.denominator() == bigint(1))
		result = _value.numerator().str();
	else
		result = to_string(_value);
	if (result.length() < _paddedLength)
		result = string(_paddedLength - result.length(), ' ') + result;
	return result;
}

vector<rational>& operator/=(vector<rational>& _data, rational const& _divisor)
{
	for (rational& x: _data)
		x /= _divisor;
	return _data;
}

vector<rational>& operator*=(vector<rational>& _data, rational const& _factor)
{
	for (rational& x: _data)
		x *= _factor;
	return _data;
}

vector<rational> operator*(rational const& _factor, vector<rational> _data)
{
	for (rational& x: _data)
		x *= _factor;
	return _data;
}

rational get(vector<rational> const& _data, size_t _index)
{
	return _index < _data.size() ? _data[_index] : 0;
}

vector<rational> operator-(vector<rational> const& _x, vector<rational> const& _y)
{
	vector<rational> result;
	for (size_t i = 0; i < max(_x.size(), _y.size()); ++i)
		result.emplace_back(get(_x, i) - get(_y, i));
	return result;
}

vector<rational> add(vector<rational> const& _x, vector<rational> const& _y)
{
	vector<rational> result;
	for (size_t i = 0; i < max(_x.size(), _y.size()); ++i)
		result.emplace_back(get(_x, i) + get(_y, i));
	return result;
}

bool isConstant(vector<rational> const& _x)
{
	return ranges::all_of(_x | ranges::views::tail, [](rational const& _v) { return _v == 0; });
}

/// Multiply two vectors where the first element of each vector is a constant factor.
/// Only works if at most one of the vector has a nonzero element after the first.
/// If this condition is violated, returns nullopt.
optional<vector<rational>> vectorProduct(optional<vector<rational>> _x, optional<vector<rational>> _y)
{
	if (!_x || !_y)
		return std::nullopt;
	if (!isConstant(*_y))
		swap(_x, _y);
	if (!isConstant(*_y))
		return std::nullopt;

	rational factor = _y->front();

	for (rational& element: *_x)
		element *= factor;
	return *_x;
}


/**
 * Simplex tableau with the first row representing the objective function.
 */
struct Tableau
{
	std::vector<std::vector<rational>> data;
};


pair<vector<Constraint>, bool> toEquationalForm(vector<Constraint> _constraints)
{
	size_t varsNeeded = static_cast<size_t>(ranges::count_if(_constraints, [](Constraint const& _c) { return !_c.equality; }));

	vector<Constraint> result;

	size_t columns = _constraints.at(0).data.size();
	size_t currentVariable = 0;
	for (Constraint& constraint: _constraints)
	{
		solAssert(constraint.data.size() == columns, "");
		result.emplace_back(Constraint{move(constraint.data) + vector<rational>(varsNeeded, bigint{}), true});
		if (!constraint.equality)
		{
			result.back().data[columns + currentVariable] = bigint(1);
			currentVariable++;
		}
	}

	return make_pair(move(result), varsNeeded > 0);
}

optional<size_t> findPivotColumn(Tableau const& _tableau)
{
	auto&& [maxColumn, maxValue] = ranges::max(
		_tableau.data[0] | ranges::views::enumerate | ranges::views::tail,
		{},
		[](std::pair<size_t, rational> const& _x) { return _x.second; }
	);

	if (maxValue <= rational{0})
		return nullopt; // found optimum
	else
		return maxColumn;
}

optional<size_t> findPivotRow(Tableau const& _tableau, size_t _pivotColumn)
{
	auto positiveColumnEntries =
		ranges::views::iota(size_t(1), _tableau.data.size()) |
		ranges::views::transform([&](size_t i) {
			return make_pair(i, _tableau.data[i][_pivotColumn]);
		}) |
		ranges::views::filter([](pair<size_t, rational> const& _entry) {
			return _entry.second > 0;
		});
	if (positiveColumnEntries.empty())
		return nullopt; // unbounded

	return ranges::min(
		positiveColumnEntries,
		{},
		[&](std::pair<size_t, rational> const& _entry) {
			return _tableau.data[_entry.first][0] / _entry.second;
		}
	).first;
}

Tableau performPivot(Tableau _tableau, size_t _pivotRow, size_t _pivotColumn)
{
	rational pivot = _tableau.data[_pivotRow][_pivotColumn];
	solAssert(pivot != 0, "");
	_tableau.data[_pivotRow] /= pivot;
	solAssert(_tableau.data[_pivotRow][_pivotColumn] == rational(1), "");

	for (size_t i = 0; i < _tableau.data.size(); ++i)
		if (i != _pivotRow)
			_tableau.data[i] = _tableau.data[i] - _tableau.data[i][_pivotColumn] * _tableau.data[_pivotRow];
	return _tableau;
}

void printVector(vector<rational> const& _v)
{
	for (auto const& element: _v)
		cout << toString(element, 3) << ", ";
	cout << endl;
}

vector<rational> optimalVector(Tableau const& _tableau);


void printTableau(Tableau _tableau)
{
	cout << "------------" << endl;
	for (auto const& row: _tableau.data)
		printVector(row);
	cout << "------------" << endl;
	cout << "Solution: ";
	printVector(optimalVector(_tableau));
}

Tableau selectLastVectorsAsBasis(Tableau _tableau)
{
	// We might skip the operation for a column if it is already the correct
	// unit vector and its cost coefficient is zero.
	size_t columns = _tableau.data.at(0).size();
	size_t rows = _tableau.data.size();
	for (size_t i = 1; i < rows; ++i)
		_tableau = performPivot(move(_tableau), i, columns - rows + i);

	return _tableau;
}

/// Returns the row containing 1 if all other rows are zero.
optional<size_t> basisVariable(Tableau const& _tableau, size_t _column)
{
	optional<size_t> row;
	for (size_t i = 1; i < _tableau.data.size(); ++i)
		if (_tableau.data[i][_column] == bigint(1))
		{
			if (row)
				return std::nullopt;
			else
				row = i;
		}
		else if (_tableau.data[i][_column] != 0)
			return std::nullopt;
	return row;
}

vector<rational> optimalVector(Tableau const& _tableau)
{
	vector<rational> result;
	set<size_t> rowsSeen;
	for (size_t j = 1; j < _tableau.data[0].size(); j++)
	{
		optional<size_t> row = basisVariable(_tableau, j);
		if (row && rowsSeen.count(*row))
			row = nullopt;
		result.emplace_back(row ? _tableau.data[*row][0] : rational{});
		if (row)
			rowsSeen.insert(*row);
	}
	//solAssert(rowsSeen.size() == _tableau.data.size() - 1, "");
	return result;
}


enum class LPResult
{
	Unknown,
	Unbounded,
	Feasible,
	Infeasible
};

/// Solve the LP Ax = b s.t. min c^Tx
/// The first row encodes the objective function
/// The first column encodes b
/// Assumes the tableau has a trivial basic feasible solution.
pair<LPResult, Tableau> simplexEq(Tableau _tableau)
{
	size_t iterations = 50 + _tableau.data[0].size() * 4;
	for (size_t step = 0; step <= iterations; ++step)
	{
		optional<size_t> pivotColumn = findPivotColumn(_tableau);
		if (!pivotColumn)
		{
			//cout << "Optimum: ";
			//vector<rational> optimum = optimalVector(_tableau);
			//printVector(optimum);

			cout << "Feasible after " << step << " steps." << endl;
			cout << "Constraints: " << (_tableau.data.size() - 1) << endl;
			cout << "Variables: " << (_tableau.data[0].size() - 1) << endl;
			return make_pair(LPResult::Feasible, move(_tableau));
		}
		//cout << "Pivot column: " << *pivotColumn << endl;
		optional<size_t> pivotRow = findPivotRow(_tableau, *pivotColumn);
		if (!pivotRow)
		{
			//cout << "unbounded" << endl;
			return make_pair(LPResult::Unbounded, move(_tableau));
		}
		//cout << "Pivot row: " << *pivotRow << endl;
		_tableau = performPivot(move(_tableau), *pivotRow, *pivotColumn);
		//cout << "After step " << step << endl;
		//printTableau(_tableau);
	}
	cout << "LP: Too many iterations: " << iterations << endl;
	return make_pair(LPResult::Unknown, Tableau{});
}

pair<LPResult, Tableau> simplexPhaseI(Tableau _tableau)
{
	vector<rational> originalObjective = _tableau.data[0];

	size_t rows = _tableau.data.size();
	size_t columns = _tableau.data.at(0).size();
	for (size_t i = 1; i < rows; ++i)
	{
		if (_tableau.data[i][0] < 0)
			_tableau.data[i] *= -1;
		_tableau.data[i] += vector<bigint>(rows - 1, bigint{});
		_tableau.data[i][columns + i - 1] = 1;
	}
	_tableau.data[0] =
		vector<rational>(columns, rational{}) +
		vector<rational>(rows - 1, rational{-1});

	//cout << "Phase I tableau: " << endl;
	//printTableau(_tableau);

	_tableau = selectLastVectorsAsBasis(move(_tableau));

	//cout << "After basis selection: " << endl;
	//printTableau(_tableau);

	LPResult result;
	tie(result, _tableau) = simplexEq(move(_tableau));

	// TODO This should actually not happen.
	if (result != LPResult::Feasible && result != LPResult::Unbounded)
	{
		cout << "Unknown because phaseI resulted in " << static_cast<int>(result) << endl;
		return make_pair(LPResult::Unknown, Tableau{});
	}
	vector<rational> optimum = optimalVector(_tableau);
	//cout << "PhaseI solution: ";
	//printVector(optimum);

	for (size_t i = columns - 1; i < optimum.size(); ++i)
		if (optimum[i] != 0)
			return make_pair(LPResult::Infeasible, Tableau{});

	_tableau.data[0] = originalObjective;
	for (size_t i = 1; i < rows; ++i)
		_tableau.data[i].resize(columns);

	//cout << "Tableau after Phase I: " << endl;
	//printTableau(_tableau);

	// TODO do we need to select a basis?
	//_tableau = selectLastVectorsAsBasis(move(_tableau));
	//cout << "After basis correction: " << endl;
	//printTableau(_tableau);

	return make_pair(LPResult::Feasible, move(_tableau));
}

bool needsPhaseI(Tableau const& _tableau)
{
	// TODO with equality constraints, this may need refinement.
	for (size_t i = 1; i < _tableau.data.size(); ++i)
		if (_tableau.data[i][0] < 0)
			return true;
	return false;
}

/// Solve the LP Ax <= b s.t. min c^Tx
/// The first row encodes the objective function
/// The first column encodes b
pair<LPResult, vector<rational>> simplex(vector<Constraint> _constraints, vector<rational> _objectives)
{
	Tableau tableau;
	tableau.data.emplace_back(move(_objectives));
	bool hasEquations = false;
	tie(_constraints, hasEquations) = toEquationalForm(_constraints);
	for (Constraint& c: _constraints)
		tableau.data.emplace_back(move(c.data));
	tableau.data.front().resize(tableau.data.at(1).size());
	cout << "Equational: " << endl;
	printTableau(tableau);
	if (hasEquations || needsPhaseI(tableau))
	{
		LPResult result;
		tie(result, tableau) = simplexPhaseI(move(tableau));
		if (result == LPResult::Infeasible || result == LPResult::Unknown)
			return make_pair(result, vector<rational>{});
		solAssert(result == LPResult::Feasible, "");
	}
	LPResult result;
	vector<rational> optimum;
	tie(result, tableau) = simplexEq(move(tableau));
	if (result == LPResult::Feasible || result == LPResult::Unbounded)
	{
		optimum = optimalVector(tableau);
		//cout << "Solution: " << endl;
		//printVector(optimum);
	}
	return make_pair(result, move(optimum));
}

}

void LPSolver::reset()
{
	m_state = stack<State>{{State{}}};
}

void LPSolver::push()
{
	m_state.push(m_state.top());
}

void LPSolver::pop()
{
	m_state.pop();
	solAssert(!m_state.empty(), "");
}

void LPSolver::declareVariable(string const& _name, SortPointer const& _sort)
{
	// TODO This will not be an integer variable in our model.
	// Introduce a new kind?
	solAssert(_sort && _sort->kind == Kind::Int, "");
	solAssert(!m_state.top().variables.count(_name), "");
	size_t index = m_state.top().variables.size() + 1;
	m_state.top().variables[_name] = index;
}

void LPSolver::addAssertion(Expression const& _expr)
{
	if (_expr.name == "and")
	{
		addAssertion(_expr.arguments.at(0));
		addAssertion(_expr.arguments.at(1));
	}
	else if (_expr.name == "<=")
	{
		// TODO If it is a direct upper or lower bound on a single variable,
		// add those to a special set, so we can directly test any contracdictians
		// and olso simplify.
		optional<vector<rational>> left = parseLinearSum(_expr.arguments.at(0));
		optional<vector<rational>> right = parseLinearSum(_expr.arguments.at(1));
		if (left && right)
		{
			vector<rational> data = *left - *right;
			data[0] *= -1;
			m_state.top().constraints.emplace_back(Constraint{move(data), false});
		}
	}
	else if (_expr.name == ">=")
		addAssertion(_expr.arguments.at(1) <= _expr.arguments.at(0));
	else if (_expr.name == "<")
		addAssertion(_expr.arguments.at(0) <= _expr.arguments.at(1) - 1);
	else if (_expr.name == ">")
		addAssertion(_expr.arguments.at(1) < _expr.arguments.at(0));
	else if (_expr.name == "=")
	{
		// TODO if a variable ends up being fixed (upper bound equal lower bound),
		// we can remove it and replace all its references.
		// this can only be done at checking time, though, as other
		// added constraints might make the system infeasible.
		// We can also leave it in and just replace everything.
		addAssertion(_expr.arguments.at(0) <= _expr.arguments.at(1));
		addAssertion(_expr.arguments.at(1) <= _expr.arguments.at(0));
	}
}

pair<CheckResult, vector<string>> LPSolver::check(vector<Expression> const& _expressionsToEvaluate)
{
	vector<Constraint> constraints = m_state.top().constraints;
	size_t numColumns = 0;
	for (auto const& row: constraints)
		numColumns = max(numColumns, row.data.size());
	for (auto& row: constraints)
		row.data.resize(numColumns);

	cout << "Solving LP:" << endl;
	for (Constraint const& constraint: constraints)
	{
		vector<string> line;
		for (auto&& [index, multiplier]: constraint.data | ranges::views::enumerate)
			if (index > 0 && multiplier != 0)
			{
				string mult =
					multiplier == -1 ?
					"-" :
					multiplier == 1 ?
					"" :
					toString(multiplier) + " ";
				line.emplace_back(mult + variableName(index));
			}
		cout << joinHumanReadable(line, " + ") << (constraint.equality ? "  =" : " <= ") << toString(constraint.data.front()) << endl;
	}
	cout << "----------------------------------------" << endl;

	bool solveInteger = false;

	CheckResult smtResult;
	LPResult lpResult;
	vector<rational> solution;
	tie(lpResult, solution) = simplex(constraints, vector<rational>(1, rational(bigint(0))) + vector<rational>(numColumns - 1, rational(bigint(1))));
	switch (lpResult)
	{
	case LPResult::Feasible:
	case LPResult::Unbounded:
		cout << "LP: feasible / unbounded." << endl;
		// We have to return "UNKNOWN" because we only solved the relaxation of the integer problem.
		// TODO We could check if the solution is integer, though.
		if (solveInteger)
			return make_pair(CheckResult::UNKNOWN, vector<string>{});
		else
			smtResult = CheckResult::SATISFIABLE;
		break;
	case LPResult::Infeasible:
		cout << "LP: infeasible." << endl;
		return make_pair(CheckResult::UNSATISFIABLE, vector<string>{});
	case LPResult::Unknown:
		cout << "LP: unknown." << endl;
		return make_pair(CheckResult::UNKNOWN, vector<string>{});
		break;
	}

	vector<string> model;
	for (Expression const& e: _expressionsToEvaluate)
	{
		if (e.arguments.empty() && m_state.top().variables.count(e.name))
		{
			size_t index = m_state.top().variables.at(e.name);
			solAssert(index > 0, "");
			model.emplace_back(toString(solution.at(index - 1), 0));
		}
		else
		{
			model = {};
			break;
		}
	}
	return make_pair(smtResult, move(model));
}

optional<vector<rational>> LPSolver::parseLinearSum(smtutil::Expression const& _expr) const
{
	if (_expr.arguments.empty() || _expr.name == "*")
		return parseProduct(_expr);
	else if (_expr.name == "+" || _expr.name == "-")
	{
		optional<vector<rational>> left = parseLinearSum(_expr.arguments.at(0));
		optional<vector<rational>> right = parseLinearSum(_expr.arguments.at(1));
		if (!left || !right)
			return std::nullopt;
		return _expr.name == "+" ? add(*left, *right) : *left - *right;
	}
	else
		return std::nullopt;
}

optional<vector<rational>> LPSolver::parseProduct(smtutil::Expression const& _expr) const
{
	if (_expr.arguments.empty())
		return parseFactor(_expr);
	else if (_expr.name == "*")
		// The multiplication ensures that only one of them can be a variable.
		return vectorProduct(parseFactor(_expr.arguments.at(0)), parseFactor(_expr.arguments.at(1)));
	else
		return std::nullopt;
}

optional<vector<rational>> LPSolver::parseFactor(smtutil::Expression const& _expr) const
{
	solAssert(_expr.arguments.empty(), "");
	solAssert(!_expr.name.empty(), "");
	if ('0' <= _expr.name[0] && _expr.name[0] <= '9')
		return vector<rational>{rational(bigint(_expr.name))};
	else if (_expr.name == "true")
		return vector<rational>{rational(bigint(1))};
	else if (_expr.name == "false")
		return vector<rational>{rational(bigint(0))};

	size_t index = m_state.top().variables.at(_expr.name);
	solAssert(index > 0, "");
	vector<rational> result(index + 1);
	result[index] = rational(bigint(1));
	return result;
}

string LPSolver::variableName(size_t _index) const
{
	for (auto const& v: m_state.top().variables)
		if (v.second == _index)
			return v.first;
	return {};
}
