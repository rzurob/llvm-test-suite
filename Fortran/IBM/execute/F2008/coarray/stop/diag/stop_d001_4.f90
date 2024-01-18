!#######################################################################
!*
!*  ===================================================================
!*
!*  TYPE                       : Diagnostic test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  DATE                       : 19 Oct 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that the error message from the
!*                               STOP statement will be printed out
!*                               on stderr and prefixed with "STOP".
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_values

  print *, " Before stop statement"
  stop 1
  print *, " After stop statement"

end program stop_values
