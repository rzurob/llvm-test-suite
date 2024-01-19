message(STATUS "Adding directory Fortran/IBM/compile/F2003/deferlen/syntax/C403")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
