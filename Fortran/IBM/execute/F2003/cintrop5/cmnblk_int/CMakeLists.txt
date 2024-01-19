message(STATUS "Adding directory Fortran/IBM/execute/F2003/cintrop5/cmnblk_int")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
