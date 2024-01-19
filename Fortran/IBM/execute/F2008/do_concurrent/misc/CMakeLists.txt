message(STATUS "Adding directory Fortran/IBM/execute/F2008/do_concurrent/misc")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
