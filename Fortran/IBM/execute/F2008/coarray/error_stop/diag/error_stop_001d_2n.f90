!#######################################################################
!*
!*  ===================================================================
!*
!*  TYPE                       : Duagnostic test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  DATE                       : 28 August 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that the error message from the
!*                               ERROR STOP statement will be printed out
!*                               on stderr and prefixed with "ERROR STOP".
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values

  print *, " Before error_stop statement"
  error stop 7

  print *, " After error_stop statement"
end program error_stop_values
