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
!*  DESCRIPTION                : Test that user does not explicitly specify
!*                               any stop-code for STOP (i.e. default one)
!*                               then the program exit status is 0
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_values

  print *, " Stop with no value set."
  stop
  sync all

  print *, " FAILURE if this message output!"

end program stop_values
