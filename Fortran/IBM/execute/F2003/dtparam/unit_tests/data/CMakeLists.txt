message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/unit_tests/data")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
