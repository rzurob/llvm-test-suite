message(STATUS "Adding directory Fortran/IBM/execute/F2003/kindArg/kindArg")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
