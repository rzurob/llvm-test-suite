message(STATUS "Adding directory Fortran/IBM/execute/F2003/OO_dtio/unformatted/syntax/C920")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
