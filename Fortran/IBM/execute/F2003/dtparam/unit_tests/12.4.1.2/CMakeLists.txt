message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/unit_tests/12.4.1.2")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
