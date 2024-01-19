message(STATUS "Adding directory Fortran/IBM/execute/F2008/coarray/coarray_access/specific/assignment")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
