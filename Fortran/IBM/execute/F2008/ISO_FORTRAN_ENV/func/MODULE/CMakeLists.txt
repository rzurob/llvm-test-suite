message(STATUS "Adding directory Fortran/IBM/execute/F2008/ISO_FORTRAN_ENV/func/MODULE")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
