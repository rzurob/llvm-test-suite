!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt34
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-31
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : vector subscript in LHS of assignment statement (derived)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : vector subscript
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Try to use a derived-type AC as a vector subscript.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt34mod

  implicit none
  type derived
     integer :: val
  end type derived

end module acetdt34mod


program acetdt34

  use acetdt34mod
  implicit none

  integer:: iarr(3)
  integer:: i
  type (derived) :: dtarr(3)

  ! Baseline - these are all okay:
  iarr = [integer:: (i ** 2, i=1,3)]
  dtarr = [derived:: (derived(i), i=3,1,-1)]
  print *, iarr(dtarr%val)
  iarr (dtarr%val) = [integer:: (i - 2, i=1,3)]
  iarr ([integer:: dtarr(1)%val]) = 99

  ! These are bad:
  iarr ([derived:: 1]) = 99
  iarr ([derived::]) = 99
  iarr ([derived:: derived(2)]) = 99

end program acetdt34
