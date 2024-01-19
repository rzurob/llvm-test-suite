message(STATUS "Adding directory Fortran/IBM/execute/F2003/asynchIO/attribute/attr")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
