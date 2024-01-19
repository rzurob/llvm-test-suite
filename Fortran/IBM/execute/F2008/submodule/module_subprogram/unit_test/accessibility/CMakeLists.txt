message(STATUS "Adding directory Fortran/IBM/execute/F2008/submodule/module_subprogram/unit_test/accessibility")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
