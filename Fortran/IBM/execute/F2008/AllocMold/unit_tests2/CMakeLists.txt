message(STATUS "Adding directory Fortran/IBM/execute/F2008/AllocMold/unit_tests2")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
