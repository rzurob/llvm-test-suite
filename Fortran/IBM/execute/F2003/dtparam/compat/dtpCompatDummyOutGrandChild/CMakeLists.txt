message(STATUS "Adding directory Fortran/IBM/execute/F2003/dtparam/compat/dtpCompatDummyOutGrandChild")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
