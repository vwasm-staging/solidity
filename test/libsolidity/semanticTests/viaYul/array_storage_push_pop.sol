contract C {
    uint[] storageArray;
    function set_get_length(uint256 len) public returns (uint256) {
        while(storageArray.length < len)
            storageArray.push();
        while(storageArray.length > 0)
            storageArray.pop();
        return storageArray.length;
    }
}
// ====
// compileViaYul: also
// ----
// set_get_length(uint256): 0 -> 0
// set_get_length(uint256): 1 -> 0
// set_get_length(uint256): 10 -> 0
// set_get_length(uint256): 20 -> 0
// gas irOptimized: 66845
// gas legacy: 62822
// gas legacyOptimized: 60608
// set_get_length(uint256): 0xFF -> 0
// gas irOptimized: 810360
// gas legacy: 766327
// gas legacyOptimized: 742258
// set_get_length(uint256): 0xFFF -> 0
// gas irOptimized: 12816532
// gas legacy: 12111559
// gas legacyOptimized: 11730370
// set_get_length(uint256): 0xFFFF -> FAILURE # Out-of-gas #
