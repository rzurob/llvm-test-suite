message(STATUS "Adding directory Fortran/IBM/execute/F2003/mProc/unit_tests/diag")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
