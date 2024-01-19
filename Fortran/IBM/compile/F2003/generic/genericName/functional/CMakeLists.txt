message(STATUS "Adding directory Fortran/IBM/compile/F2003/generic/genericName/functional")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
