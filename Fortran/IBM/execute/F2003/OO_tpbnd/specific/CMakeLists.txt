message(STATUS "Adding directory Fortran/IBM/execute/F2003/OO_tpbnd/specific")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
