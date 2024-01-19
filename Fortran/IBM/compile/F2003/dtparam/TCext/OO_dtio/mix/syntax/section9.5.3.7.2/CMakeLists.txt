message(STATUS "Adding directory Fortran/IBM/compile/F2003/dtparam/TCext/OO_dtio/mix/syntax/section9.5.3.7.2")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
