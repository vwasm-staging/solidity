contract C {
    uint[] storageArray;
    function test_zeroed_indicies(uint256 len) public
    {
        while(storageArray.length < len)
            storageArray.push();
        while(storageArray.length > len)
            storageArray.pop();

        for (uint i = 0; i < len; i++)
            storageArray[i] = i + 1;

        if (len > 3)
        {
            while(storageArray.length > 0)
                storageArray.pop();
            while(storageArray.length < 3)
                storageArray.push();

            for (uint i = 3; i < len; i++)
            {
                assembly {
                    mstore(0, storageArray.slot)
                    let pos := add(keccak256(0, 0x20), i)

                    if iszero(eq(sload(pos), 0)) {
                        revert(0, 0)
                    }
                }
            }

        }

        while(storageArray.length > 0)
            storageArray.pop();
        while(storageArray.length < len)
            storageArray.push();

        for (uint i = 0; i < len; i++)
        {
            require(storageArray[i] == 0);

            uint256 val = storageArray[i];
            uint256 check;

            assembly { check := iszero(val) }

            require(check == 1);
        }
    }
}
// ====
// compileViaYul: also
// ----
// test_zeroed_indicies(uint256): 1 ->
// test_zeroed_indicies(uint256): 5 ->
// gas irOptimized: 129918
// gas legacy: 128267
// gas legacyOptimized: 125486
// test_zeroed_indicies(uint256): 10 ->
// gas irOptimized: 167646
// gas legacy: 164829
// gas legacyOptimized: 159724
// test_zeroed_indicies(uint256): 15 ->
// gas irOptimized: 182956
// gas legacy: 178954
// gas legacyOptimized: 171604
// test_zeroed_indicies(uint256): 0xFF ->
// gas irOptimized: 6186066
// gas legacy: 6129649
// gas legacyOptimized: 5995974
