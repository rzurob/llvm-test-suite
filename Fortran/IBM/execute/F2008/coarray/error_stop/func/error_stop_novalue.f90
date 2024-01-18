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
!*  DESCRIPTION                : Test that user does not explicitly specify
!*                               any stop-code for ERROR STOP (i.e. default one)
!*                               then the program exit status is 1
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values

  print *, " Error Stop with no value set."
  error stop
  sync all

  print *, " FAILURE if this message output!"
  sync all
end program error_stop_values
