!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint34
!*
!*  DATE                       : 2006-10-31
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : vector subscript in LHS of assignment statement (intrinsic)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : vector subscript, diagnostic
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Two intrinsic types are clearly illegal as indexes: character and logical.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint34

  implicit none

  integer:: iarr(3), i

  ! Baseline - these are all okay:
  iarr = [integer:: (i ** 2, i=1,3)]

  ! These should both be illegal:
  iarr([character:: 'a', 'b']) = 66
  iarr([logical:: .true., .false.]) = 66

end program acetint34
