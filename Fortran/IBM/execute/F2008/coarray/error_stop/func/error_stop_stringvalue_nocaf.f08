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
!*  DESCRIPTION                : Test that user explicitly specify non integer
!*                               but string value for ERROR STOP
!*                               then the program exit status is 1
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values

  print *, " Error Stop with string value set."
  error stop "ES_String message!"

  print *, " FAILURE if this message output!"
end program error_stop_values