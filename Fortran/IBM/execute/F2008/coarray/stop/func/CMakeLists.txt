message(STATUS "Adding directory Fortran/IBM/execute/F2008/coarray/stop/func")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
