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
!*  DESCRIPTION                : Test that user explicitly specify non integer
!*                               but string value for STOP
!*                               then the program exit status is 0
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_values

  print *, " Stop with string value set."
  stop "ES_String message!"

  print *, " FAILURE if this message output!"
  sync all
end program stop_values
