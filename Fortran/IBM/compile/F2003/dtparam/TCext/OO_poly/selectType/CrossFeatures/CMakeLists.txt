message(STATUS "Adding directory Fortran/IBM/compile/F2003/dtparam/TCext/OO_poly/selectType/CrossFeatures")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
