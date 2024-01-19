message(STATUS "Adding directory Fortran/IBM/compile/F2003/dtparam/TCext/OO_dtio/formatted/syntax/C916")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
