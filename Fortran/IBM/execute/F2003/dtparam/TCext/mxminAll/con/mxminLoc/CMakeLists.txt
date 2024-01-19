message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/TCext/mxminAll/con/mxminLoc")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
