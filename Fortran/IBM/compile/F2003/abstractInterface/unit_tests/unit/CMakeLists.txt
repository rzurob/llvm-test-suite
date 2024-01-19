message(STATUS "Adding directory Fortran/IBM/compile/F2003/abstractInterface/unit_tests/unit")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
