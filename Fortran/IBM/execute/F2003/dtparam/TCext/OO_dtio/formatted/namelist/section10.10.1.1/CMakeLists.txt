message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/TCext/OO_dtio/formatted/namelist/section10.10.1.1")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
