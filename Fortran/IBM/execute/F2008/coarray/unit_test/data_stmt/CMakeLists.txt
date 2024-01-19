message(STATUS "Adding directory Fortran/IBM/execute/F2008/coarray/unit_test/data_stmt")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
