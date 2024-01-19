message(STATUS "Adding directory Fortran/IBM/compile/F2008/intrinsics/csizeof/unit_tests/diag")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
