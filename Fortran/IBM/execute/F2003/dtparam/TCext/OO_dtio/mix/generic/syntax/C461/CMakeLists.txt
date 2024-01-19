message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/TCext/OO_dtio/mix/generic/syntax/C461")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
