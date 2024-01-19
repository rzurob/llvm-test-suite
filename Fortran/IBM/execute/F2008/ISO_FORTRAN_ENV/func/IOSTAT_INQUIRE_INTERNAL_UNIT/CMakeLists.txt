message(STATUS "Adding directory Fortran/IBM/execute/F2008/ISO_FORTRAN_ENV/func/IOSTAT_INQUIRE_INTERNAL_UNIT")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
