message(STATUS "Adding directory Fortran/IBM/execute/F2003/asynchIO/statements/dtio")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
