message(STATUS "Adding directory Fortran/IBM/compile/F2003/dtparam/dummy_arg/deferlen")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
