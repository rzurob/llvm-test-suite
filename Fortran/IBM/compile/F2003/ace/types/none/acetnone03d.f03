!******************************************************************************
!*  ===========================================================================
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

program acenone03d

  use mod
  implicit none

  integer    :: i
  integer    :: iarr  (4)
  real       :: rarr  (3)
  complex    :: zarr  (3)
  logical    :: larr  (4)

  type (usertype)   :: ut(2)
  type (usertypeB)  :: utb(2)

  iarr = (/ (int(i,1),int(i,2),int(i,4),int(i,8),i=1,1) /)
  iarr = (/ (int(i,1),int(i,2),i=1,1), (int(i,4),int(i,8),i=1,1) /)
  rarr = (/ (real(i,4),real(i,8),real(i,16),i=2,2) /)
  rarr = (/ (real(i,4),i=1,3), (real(i,16),real(i,16),i=1,0) /)
  carr = (/ (complx(i,i,4),complx(i,i,8),complx(i,i,16),i=9,9) /)
  carr = (/ (complx(i,i,4),i=1,3), (complx(i,i,16),i=9,0) /)

  larr = (/ (logical(i==1,1),i=1,2), (logical(i==2,2),i=2,3) /)
  larr = (/ (logical(i==1,4),i=1,4), (logical(i==2,8),i=2,0) /)

  ut = (/ (usertype(i),usertypeB(i),i=1,1) /)
  ut = (/ (usertypeB(i),usertype(i),i=1,1) /)
  ut = (/ (usertype(i),i=1,2), (usertypeB(i),i=1,0) /)
  utb = (/ (usertypeB(i),usertype(i),i=1,1) /)
  utb = (/ (usertype(i),usertypeB(i),i=1,1) /)
  utb = (/ (usertypeB(i),i=1,2), (usertype(i),i=1,0) /)

end program acenone03d
