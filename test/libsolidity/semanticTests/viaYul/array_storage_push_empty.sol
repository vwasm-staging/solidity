contract C {
    uint256[] storageArray;
    function pushEmpty(uint256 len) public {
        while(storageArray.length < len)
            storageArray.push();

        for (uint i = 0; i < len; i++)
            require(storageArray[i] == 0);
    }
}
// ====
// EVMVersion: >=petersburg
// compileViaYul: also
// ----
// pushEmpty(uint256): 128
// gas irOptimized: 430912
// gas legacy: 417287
// gas legacyOptimized: 399048
// pushEmpty(uint256): 256
// gas irOptimized: 474164
// gas legacy: 457083
// gas legacyOptimized: 430908
// pushEmpty(uint256): 32768 -> # out-of-gas #
// gas irOptimized: 98565684
// gas legacy: 95047675
// gas legacyOptimized: 90447420
