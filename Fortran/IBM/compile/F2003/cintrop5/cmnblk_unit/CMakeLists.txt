message(STATUS "Adding directory Fortran/IBM/compile/F2003/cintrop5/cmnblk_unit")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
