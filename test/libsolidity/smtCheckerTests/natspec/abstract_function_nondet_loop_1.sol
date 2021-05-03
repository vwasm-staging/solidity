contract C {
	uint[] vs;

	uint x;

	constructor(uint _x) {
		require(_x > 0);
		x = _x;
	}

	function e(uint _e) public {
		x = pow(x, _e);
		vs.push(x % 2);
	}

	function inv() public view {
		if (vs.length == 0)
			return;
		assert(vs[0] < 2);
	}

    function pow(uint base, uint exponent) internal pure returns (uint) {
        if (base == 0) {
            return 0;
        }
        if (exponent == 0) {
            return 1;
        }
        if (exponent == 1) {
            return base;
        }
        uint y = 1;
        while(exponent > 1) {
            if(exponent % 2 == 0) {
                base = base * base;
                exponent = exponent / 2;
            } else {
                y = base * y;
                base = base * base;
                exponent = (exponent - 1) / 2;
            }
        }
        return base * y;
    }
}
