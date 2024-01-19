!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-29 (original: 2006-07-27)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement C494: values
!*                               must have same type and values if no type spec
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array constructor, diagnostic
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  "C494 (R466) If type-spec is omitted, each ac-value expression in the array-
!*               constructor shall have the same type and kind type parameters."
!*
!*  Use a mixture of types in the ac-implied-do; the compiler should flag these
!*  as errors.
!*  Identical to acetnone03, but using square brackets in place of (/ /).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  implicit none

  type usertype
     integer(4) :: val
  end type usertype

  type usertypeB (lusertypeB_1,kusertypeB_1) ! lusertypeB_1,kusertypeB_1=8,4
     integer, len :: lusertypeB_1
     integer, kind :: kusertypeB_1
     integer(kusertypeB_1) :: valB
  end type usertypeB

end module mod

program acenone03s

  use mod
  implicit none

  integer    :: i
  integer    :: iarr  (4)
  real       :: rarr  (3)
  complex    :: zarr  (3)
  logical    :: larr  (4)

  type (usertype)   :: ut(2)
  type (usertypeB(8,4))  :: utb(2) ! tcx: (8,4)

  iarr = [ (int(i,1),i=1,4) ]
  iarr = [ (int(i,2),i=1,4) ]
  iarr = [ (int(i,4),i=1,4) ]
  iarr = [ (int(i,8),i=1,4) ]

  rarr = [ (real(i,4),i=1,3) ]
  rarr = [ (real(i,8),i=1,3) ]
  rarr = [ (real(i,16),i=1,3) ]

  zarr = [ (cmplx(i,i,4),i=1,3) ]
  zarr = [ (cmplx(i,i,8),i=1,3) ]
  zarr = [ (cmplx(i,i,16),i=1,3) ]

  larr = [ (logical(i==1,1),i=1,4) ]
  larr = [ (logical(i==2,2),i=1,4) ]
  larr = [ (logical(i==1,4),i=1,4) ]
  larr = [ (logical(i==2,8),i=1,4) ]

  ut = [ (usertype(i),i=1,2) ]
  utb = [ (usertypeB(8,4)(i),i=1,2) ] ! tcx: (8,4)

end program acenone03s


! Extensions to introduce derived type parameters:
! type: usertypeB - added parameters (lusertypeB_1,kusertypeB_1) to invoke with (8,4)/declare with (8,4) - 2 changes
