contract C {
    uint[] storageArray;
    function set_get_length(uint256 len) public returns (uint256) {
        while(storageArray.length < len)
            storageArray.push();
        return storageArray.length;
    }
}
// ====
// compileViaYul: also
// ----
// set_get_length(uint256): 0 -> 0
// set_get_length(uint256): 1 -> 1
// set_get_length(uint256): 10 -> 10
// set_get_length(uint256): 20 -> 20
// set_get_length(uint256): 0xFF -> 0xFF
// gas irOptimized: 103956
// gas legacy: 124722
// gas legacyOptimized: 105818
// set_get_length(uint256): 0xFFF -> 0xFFF
// gas irOptimized: 1365718
// gas legacy: 1700119
// gas legacyOptimized: 1396420
// set_get_length(uint256): 0xFFFFF -> FAILURE # Out-of-gas #
