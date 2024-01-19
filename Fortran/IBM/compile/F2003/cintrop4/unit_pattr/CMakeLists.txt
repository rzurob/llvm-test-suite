message(STATUS "Adding directory Fortran/IBM/compile/F2003/cintrop4/unit_pattr")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
