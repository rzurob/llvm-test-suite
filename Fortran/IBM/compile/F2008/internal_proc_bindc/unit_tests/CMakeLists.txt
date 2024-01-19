message(STATUS "Adding directory Fortran/IBM/compile/F2008/internal_proc_bindc/unit_tests")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
