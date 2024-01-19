message(STATUS "Adding directory Fortran/IBM/compile/F2008/coarray/intrinsics/num_images/diag")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_compile_tests("${TESTS}")
