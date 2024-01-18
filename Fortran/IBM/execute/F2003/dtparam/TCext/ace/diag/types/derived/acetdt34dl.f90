! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/diag/types/derived/acetdt34d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt34
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt34d
!*                               by David Forster)
!*  DATE                       : 2007-11-20 (original: 2006-10-31)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt34mod

  implicit none
  type derived(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     integer(k1)   :: val
  end type derived

end module acetdt34mod


program acetdt34

  use acetdt34mod
  implicit none

  integer:: iarr(3)
  integer:: i
  type (derived(20,4)) :: dtarr(3)

  ! Baseline - these are all okay:
  iarr = [integer:: (i ** 2, i=1,3)]
  dtarr = [derived(20,4):: (derived(20,4)(i), i=3,1,-1)]
  print *, iarr(dtarr%val)
  iarr (dtarr%val) = [integer:: (i - 2, i=1,3)]
  iarr ([integer:: dtarr(1)%val]) = 99

  ! These are bad:
  iarr ([derived(20,4):: 1]) = 99
  iarr ([derived(20,4)::]) = 99
  iarr ([derived(20,4):: derived(20,4)(2)]) = 99

end program acetdt34
