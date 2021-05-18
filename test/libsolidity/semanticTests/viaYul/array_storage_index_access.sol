contract C {
    uint[] storageArray;
    function test_indices(uint256 len) public
    {
        while (storageArray.length < len)
            storageArray.push();
        while (storageArray.length > len)
            storageArray.pop();
        for (uint i = 0; i < len; i++)
            storageArray[i] = i + 1;

        for (uint i = 0; i < len; i++)
            require(storageArray[i] == i + 1);
    }
}
// ====
// compileViaYul: also
// ----
// test_indices(uint256): 1 ->
// test_indices(uint256): 129 ->
// gas irOptimized: 3066168
// gas legacy: 3067105
// gas legacyOptimized: 3007773
// test_indices(uint256): 5 ->
// gas irOptimized: 101556
// gas legacy: 96341
// gas legacyOptimized: 93249
// test_indices(uint256): 10 ->
// test_indices(uint256): 15 ->
// gas irOptimized: 44670
// test_indices(uint256): 0xFF ->
// gas irOptimized: 3251240
// gas legacy: 3241267
// gas legacyOptimized: 3125207
// test_indices(uint256): 1000 ->
// gas irOptimized: 18079162
// gas legacy: 18080499
// gas legacyOptimized: 17641444
// test_indices(uint256): 129 ->
// gas irOptimized: 709683
// gas legacy: 670735
// gas legacyOptimized: 614547
// test_indices(uint256): 128 ->
// gas irOptimized: 195897
// gas legacy: 194372
// gas legacyOptimized: 145524
// test_indices(uint256): 1 ->
// gas irOptimized: 97899
// gas legacy: 92607
// gas legacyOptimized: 91011
