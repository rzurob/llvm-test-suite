message(STATUS "Adding directory Fortran/IBM/execute/F2008/coarray/error_stop/cnst_expr")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
