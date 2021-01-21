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
#include <libsmtutil/Sorts.h>
#include <libsolutil/StringUtils.h>
#include <test/Common.h>

#include <boost/test/unit_test.hpp>

using namespace std;
using namespace solidity::smtutil;
using namespace solidity::util;


namespace solidity::util::test
{

class LPTestFramework
{
protected:
	LPSolver solver;

	Expression variable(string const& _name)
	{
		return solver.newVariable(_name, smtutil::SortProvider::sintSort);
	}

	void feasible(vector<pair<Expression, string>> const& _solution)
	{
		vector<Expression> variables;
		vector<string> values;
		for (auto const& [var, val]: _solution)
		{
			variables.emplace_back(var);
			values.emplace_back(val);
		}
		auto [result, model] = solver.check(variables);
		BOOST_CHECK(result == smtutil::CheckResult::SATISFIABLE);
		BOOST_CHECK_EQUAL(joinHumanReadable(model), joinHumanReadable(values));
	}

	void infeasible()
	{
		auto [result, model] = solver.check({});
		BOOST_CHECK(result == smtutil::CheckResult::UNSATISFIABLE);
	}
};


BOOST_FIXTURE_TEST_SUITE(LP, LPTestFramework, *boost::unit_test::label("nooptions"))

BOOST_AUTO_TEST_CASE(basic)
{
	Expression x = variable("x");
	solver.addAssertion(2 * x <= 10);
	feasible({{x, "5"}});
}

BOOST_AUTO_TEST_CASE(not_linear_independent)
{
	Expression x = variable("x");
	solver.addAssertion(2 * x <= 10 && 4 * x <= 20);
	feasible({{x, "5"}});
}

BOOST_AUTO_TEST_CASE(two_vars)
{
	Expression x = variable("x");
	Expression y = variable("y");
	solver.addAssertion(y <= 3);
	solver.addAssertion(x <= 10);
	solver.addAssertion(4 >= x + y);
	feasible({{x, "1"}, {y, "3"}});
}

BOOST_AUTO_TEST_CASE(lower_bound)
{
	Expression x = variable("x");
	Expression y = variable("y");
	solver.addAssertion(y >= 1);
	solver.addAssertion(x <= 10);
	solver.addAssertion(2 * x + y <= 2);
	feasible({{x, "0"}, {y, "2"}});
}

BOOST_AUTO_TEST_CASE(check_infeasible)
{
	Expression x = variable("x");
	solver.addAssertion(x <= 3 && x >= 5);
	infeasible();
}

BOOST_AUTO_TEST_CASE(unbounded)
{
	Expression x = variable("x");
	solver.addAssertion(x >= 2);
	// TODO the smt checker does not expose a status code of "unbounded"
	feasible({{x, "2"}});
}

BOOST_AUTO_TEST_CASE(unbounded_two)
{
	Expression x = variable("x");
	Expression y = variable("y");
	solver.addAssertion(x + y >= 2);
	solver.addAssertion(x <= 10);
	feasible({{x, "10"}, {y, "0"}});
}

BOOST_AUTO_TEST_CASE(equal)
{
	Expression x = variable("x");
	Expression y = variable("y");
	solver.addAssertion(x == y + 10);
	solver.addAssertion(x <= 20);
	feasible({{x, "20"}, {y, "10"}});
}

BOOST_AUTO_TEST_CASE(push_pop)
{
	Expression x = variable("x");
	Expression y = variable("y");
	solver.addAssertion(x + y <= 20);
	feasible({{x, "20"}, {y, "0"}});

	solver.push();
	solver.addAssertion(x <= 5);
	solver.addAssertion(y <= 5);
	feasible({{x, "5"}, {y, "5"}});

	solver.push();
	solver.addAssertion(x >= 7);
	infeasible();
	solver.pop();

	feasible({{x, "5"}, {y, "5"}});
	solver.pop();

	feasible({{x, "20"}, {y, "0"}});
}

BOOST_AUTO_TEST_CASE(less_than)
{
	Expression x = variable("x");
	Expression y = variable("y");
	solver.addAssertion(x == y + 1);
	solver.push();
	solver.addAssertion(y < x);
	feasible({{x, "1"}, {y, "0"}});
	solver.pop();
	solver.push();
	solver.addAssertion(y > x);
	infeasible();
	solver.pop();
}

BOOST_AUTO_TEST_CASE(equal_constant)
{
	Expression x = variable("x");
	Expression y = variable("y");
	solver.addAssertion(x < y);
	solver.addAssertion(y == 5);
	feasible({{x, "4"}, {y, "5"}});
}

BOOST_AUTO_TEST_CASE(chained_less_than)
{
	Expression x = variable("x");
	Expression y = variable("y");
	Expression z = variable("z");
	solver.addAssertion(x < y && y < z);

	solver.push();
	solver.addAssertion(z == 0);
	infeasible();
	solver.pop();

	solver.push();
	solver.addAssertion(z == 1);
	infeasible();
	solver.pop();

	solver.push();
	solver.addAssertion(z == 2);
	feasible({{x, "0"}, {y, "1"}, {z, "2"}});
	solver.pop();

}


BOOST_AUTO_TEST_SUITE_END()

}
