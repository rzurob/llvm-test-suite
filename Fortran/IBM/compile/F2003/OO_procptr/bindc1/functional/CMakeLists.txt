message(STATUS "Adding directory Fortran/IBM/compile/F2003/OO_procptr/bindc1/functional")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
