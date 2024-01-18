!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-29 (original: 2006-07-26)
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
!*
!*  Changes: originally written 06-16, revised and renamed 07-26 - subroutines
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  implicit none

  type usertype (kusertype_1) ! kusertype_1=4
     integer, kind :: kusertype_1
     integer(kusertype_1) :: val
  end type usertype

  type usertypeB (lusertypeB_1) ! lusertypeB_1=2
     integer, len :: lusertypeB_1
     integer(4) :: valB
  end type usertypeB

end module mod

program acenone03

  use mod
  implicit none

  integer    :: i
  integer    :: iarr  (4)
  real       :: rarr  (3)
  complex    :: zarr  (3)
  logical    :: larr  (4)

  type (usertype(4))   :: ut(2) ! tcx: (4)
  type (usertypeB(2))  :: utb(2) ! tcx: (2)

  iarr = (/ (int(i,1),i=1,4) /)
  iarr = (/ (int(i,2),i=1,4) /)
  iarr = (/ (int(i,4),i=1,4) /)
  iarr = (/ (int(i,8),i=1,4) /)

  rarr = (/ (real(i,4),i=1,3) /)
  rarr = (/ (real(i,8),i=1,3) /)
  rarr = (/ (real(i,16),i=1,3) /)

  zarr = (/ (cmplx(i,i,4),i=1,3) /)
  zarr = (/ (cmplx(i,i,8),i=1,3) /)
  zarr = (/ (cmplx(i,i,16),i=1,3) /)

  larr = (/ (logical(i==1,1),i=1,4) /)
  larr = (/ (logical(i==2,2),i=1,4) /)
  larr = (/ (logical(i==1,4),i=1,4) /)
  larr = (/ (logical(i==2,8),i=1,4) /)

  ut = (/ (usertype(4)(i),i=1,2) /) ! tcx: (4)
  utb = (/ (usertypeB(2)(i),i=1,2) /) ! tcx: (2)

end program acenone03


! Extensions to introduce derived type parameters:
! type: usertype - added parameters (kusertype_1) to invoke with (4)/declare with (4) - 2 changes
! type: usertypeB - added parameters (lusertypeB_1) to invoke with (2)/declare with (2) - 2 changes
