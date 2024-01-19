!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  DATE                       : 28 August 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that on sequential (non-CAF) mode,
!*                               program exit status is the same as the
!*                               supplied stop-code value in the ERROR STOP statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values

  print *, " Error Code Value set to: ", TC_ES_VALUE
  error stop TC_ES_VALUE

  print *, " FAILURE if this message output!"
end program error_stop_values
