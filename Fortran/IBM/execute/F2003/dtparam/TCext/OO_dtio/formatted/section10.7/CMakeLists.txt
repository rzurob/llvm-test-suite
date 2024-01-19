message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/TCext/OO_dtio/formatted/section10.7")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
