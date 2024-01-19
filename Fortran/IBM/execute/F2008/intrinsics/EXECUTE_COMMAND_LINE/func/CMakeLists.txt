message(STATUS "Adding directory Fortran/IBM/execute/F2008/intrinsics/EXECUTE_COMMAND_LINE/func")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
