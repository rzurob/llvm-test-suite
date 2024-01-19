message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/TCext/dataPtrAssgn/intrinsic")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
