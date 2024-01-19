message(STATUS "Adding directory Fortran/IBM/compile/F2003/OO_poly/func_return/diag")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
