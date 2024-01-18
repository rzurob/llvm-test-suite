!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acenone03
!*
!*  DATE                       : 2006-07-26
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : C494: values must have same type and values if no type spec
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array constructor, diagnostic
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  "C494 (R466) If type-spec is omitted, each ac-value expression in the array-
!*               constructor shall have the same type and kind type parameters."
!*
!*  Use a mixture of types in the ac-implied-do; the compiler should flag these as errors.
!*
!*  Changes: originally written 06-16, revised and renamed 07-26 - subroutines
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module mod
  implicit none

  type usertype
     integer(4) :: val
  end type usertype

  type usertypeB
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

  type (usertype)   :: ut(2)
  type (usertypeB)  :: utb(2)

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

  ut = (/ (usertype(i),i=1,2) /)
  utb = (/ (usertypeB(i),i=1,2) /)

end program acenone03
