message(STATUS "Adding directory Fortran/IBM/execute/F2008/internal_proc_bindc/func")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
