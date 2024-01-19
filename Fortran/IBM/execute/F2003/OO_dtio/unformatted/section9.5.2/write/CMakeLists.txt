message(STATUS "Adding directory Fortran/IBM/execute/F2003/OO_dtio/unformatted/section9.5.2/write")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
