!#######################################################################
!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  DATE                       : 19 Oct 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that on sequential (non-CAF) mode,
!*                               program exit status is the same as the
!*                               supplied stop-code value in the STOP statement
!*                               (if the stop-code is an integer)
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_values

  print *, " Error Code Value set to: ", TC_STOP_VALUE
  stop TC_STOP_VALUE

  print *, " FAILURE if this message output!"

end program stop_values
