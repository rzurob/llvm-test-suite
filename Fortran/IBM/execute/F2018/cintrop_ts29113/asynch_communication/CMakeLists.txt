message(STATUS "Adding directory Fortran/IBM/execute/F2018/cintrop_ts29113/asynch_communication")

set(TESTS)
ibm_populate_tests(TESTS)
ibm_add_execute_tests("${TESTS}")
