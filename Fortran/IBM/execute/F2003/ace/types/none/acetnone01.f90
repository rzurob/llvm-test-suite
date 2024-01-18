!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-26
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : C494: values must have same type and values if no type spec
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : array constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  "C494 (R466) If type-spec is omitted, each ac-value expression in the array-
!*               constructor shall have the same type and kind type parameters."
!*
!*  Verify that using only the same types in the AC is permitted.
!*
!*  Changes: originally written 06-16, revised and renamed 07-26 - just assign
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

program acetnone01

  use mod
  implicit none

  integer    :: iarr  (3)
  integer(1) :: iarr1 (3)
  integer(2) :: iarr2 (3)
  integer(4) :: iarr4 (3)
  integer(8) :: iarr8 (3)

  real       :: rarr  (3)
  real(4)    :: rarr4 (3)
  real(8)    :: rarr8 (3)
  real(16)   :: rarr16(3)

  complex    :: zarr  (3)
  complex(4) :: zarr4 (3)
  complex(8) :: zarr8 (3)
  complex(16):: zarr16(3)

  logical    :: larr  (2)
  logical(1) :: larr1 (2)
  logical(2) :: larr2 (2)
  logical(4) :: larr4 (2)
  logical(8) :: larr8 (2)

  character   :: charr (3)
  character(1):: charr1(3)
  character(2):: charr2(3)

  type (usertype)   :: ut(3)
  type (usertypeB)  :: utb(3)


  iarr   = (/ -1, 0, 1 /)
  iarr1  = (/ -1_1, 0_1, 1_1 /)
  iarr2  = (/ -1_2, 0_2, 1_2 /)
  iarr4  = (/ -1_4, 0_4, 1_4 /)
  iarr8  = (/ -1_8, 0_8, 1_8 /)

  rarr   = (/ -1.0, 0.0, 1.0 /)
  rarr4  = (/ -1.0_4,  0.0_4,  1.0_4 /)
  rarr8  = (/ -1.0_8,  0.0_8,  1.0_8 /)
  rarr16 = (/ -1.0_16, 0.0_16, 1.0_16 /)

  zarr   = (/ (-1.0,0.0),      (0.0,1.0),      (1.0,1.0) /)
  zarr4  = (/ (-1.0_4,0.0_4),  (0.0_4,1.0_4),  (1.0_4,1.0_4) /)
  zarr8  = (/ (-1.0_8,0.0_8),  (0.0_8,1.0_8),  (1.0_8,1.0_8) /)
  zarr16 = (/ (-1.0_16,0.0_16),(0.0_16,1.0_16),(1.0_16,1.0_16)/)

  larr   = (/ .true., .false. /)
  larr1  = (/ .true._1, .false._1 /)
  larr2  = (/ .true._2, .false._2 /)
  larr4  = (/ .true._4, .false._4 /)
  larr8  = (/ .true._8, .false._8 /)

  charr  = (/ 'a', 'b', 'c' /)
  charr1 = (/ 1_'d', 1_'e', 1_'f' /)
  charr2 = (/ 'gh', 'hi', 'ij' /)

  ut     = (/ usertype(1), usertype(2), usertype(3) /)
  utb    = (/ usertypeB(1), usertypeB(2), usertypeB(3) /)

end program acetnone01
