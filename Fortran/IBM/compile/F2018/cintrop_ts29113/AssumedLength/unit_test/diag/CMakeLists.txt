message(STATUS "Adding directory Fortran/IBM/compile/F2018/cintrop_ts29113/AssumedLength/unit_test/diag")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
