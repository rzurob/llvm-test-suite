message(STATUS "Adding directory Fortran/IBM/execute/F2003/OO_dtio/mix/generic/functional/crossFeature/selectType")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
