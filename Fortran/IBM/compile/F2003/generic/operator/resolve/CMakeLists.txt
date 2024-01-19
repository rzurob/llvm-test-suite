message(STATUS "Adding directory Fortran/IBM/compile/F2003/generic/operator/resolve")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
