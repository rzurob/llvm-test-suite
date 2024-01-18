! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/types/none/acetnone01d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetnone01dl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetnone01d
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-07-26)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array constructor, diagnostic
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  "C494 (R466) If type-spec is omitted, each ac-value expression in the array-
!*  constructor shall have the same type and kind type parameters."
!*
!*  Use a mixture of types in the AC; the compiler should flag these as errors.
!*  Keep it simple: pairs of mismatched types in single constructors, rather
!*  than many different types.  Also, verify that the ordering doesn't matter,
!*  simply by trying both orderings.
!*
!*  Changes: originally written 06-16, revised and renamed 07-26 - just assign
!*
!23456789012345678901234567890123456789012345678901234567890123456789012


module mod
  implicit none

  type usertype(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     integer(k1)   :: val
  end type usertype

  type usertypeB(l2,k2)    ! (20,4)
     integer, kind :: k2
     integer, len  :: l2
     integer(k2)   :: valB
  end type usertypeB

end module mod

program acetnone01dl

  use mod
  implicit none

  integer    :: iarr  (2)
  real       :: rarr  (2)
  complex    :: zarr  (2)
  logical    :: larr  (2)
  character(2):: charr (2)

  type (usertype(20,4))   :: ut(2)
  type (usertypeB(20,4))  :: utb(2)

  ! Depending on -qintsize, 0 and 0_4 may not have the same kind.
  iarr = (/ 0, 0_1 /)
  iarr = (/ 0, 0_2 /)
  iarr = (/ 0, 0_4 /)
  iarr = (/ 0, 0_8 /)

  iarr = (/ 0_1, 0 /)
  iarr = (/ 0_1, 0_2 /)
  iarr = (/ 0_1, 0_4 /)
  iarr = (/ 0_1, 0_8 /)

  iarr = (/ 0_2, 0 /)
  iarr = (/ 0_2, 0_1 /)
  iarr = (/ 0_2, 0_4 /)
  iarr = (/ 0_2, 0_8 /)

  iarr = (/ 0_4, 0 /)
  iarr = (/ 0_4, 0_1 /)
  iarr = (/ 0_4, 0_2 /)
  iarr = (/ 0_4, 0_8 /)

  iarr = (/ 0_8, 0 /)
  iarr = (/ 0_8, 0_1 /)
  iarr = (/ 0_8, 0_2 /)
  iarr = (/ 0_8, 0_4 /)


  ! Depending on -qrealsize, 0.0 and 0.0_4 may not have the same kind.
  rarr = (/ 0.0, 0.0_4 /)
  rarr = (/ 0.0, 0.0_8 /)
  rarr = (/ 0.0, 0.0_16 /)

  rarr = (/ 0.0_4, 0.0 /)
  rarr = (/ 0.0_4, 0.0_8 /)
  rarr = (/ 0.0_4, 0.0_16 /)

  rarr = (/ 0.0_8, 0.0 /)
  rarr = (/ 0.0_8, 0.0_4 /)
  rarr = (/ 0.0_8, 0.0_16 /)

  rarr = (/ 0.0_16, 0.0 /)
  rarr = (/ 0.0_16, 0.0_4 /)
  rarr = (/ 0.0_16, 0.0_8 /)


  ! Only check complex numbers where the real and imaginary parts match.
  zarr = (/ (0.0,0.0), (1.0_4,1.0_4) /)
  zarr = (/ (0.0,0.0), (1.0_8,1.0_8) /)
  zarr = (/ (0.0,0.0), (1.0_16,1.0_16) /)

  zarr = (/ (0.0_4,0.0_4), (1.0,1.0) /)
  zarr = (/ (0.0_4,0.0_4), (1.0_8,1.0_8) /)
  zarr = (/ (0.0_4,0.0_4), (1.0_16,1.0_16) /)

  zarr = (/ (0.0_8,0.0_8), (1.0,1.0) /)
  zarr = (/ (0.0_8,0.0_8), (1.0_4,1.0_4) /)
  zarr = (/ (0.0_8,0.0_8), (1.0_16,1.0_16) /)

  zarr = (/ (0.0_16,0.0_16), (1.0,1.0) /)
  zarr = (/ (0.0_16,0.0_16), (1.0_4,1.0_4) /)
  zarr = (/ (0.0_16,0.0_16), (1.0_8,1.0_8) /)


  larr = (/ .true.,   .false._1 /)
  larr = (/ .true.,   .false._2 /)
  larr = (/ .true.,   .false._4 /)
  larr = (/ .true.,   .false._8 /)

  larr = (/ .true._1, .false.   /)
  larr = (/ .true._1, .false._2 /)
  larr = (/ .true._1, .false._4 /)
  larr = (/ .true._1, .false._8 /)

  larr = (/ .true._2, .false.   /)
  larr = (/ .true._2, .false._1 /)
  larr = (/ .true._2, .false._4 /)
  larr = (/ .true._2, .false._8 /)

  larr = (/ .true._4, .false.   /)
  larr = (/ .true._4, .false._1 /)
  larr = (/ .true._4, .false._2 /)
  larr = (/ .true._4, .false._8 /)

  larr = (/ .true._8, .false.   /)
  larr = (/ .true._8, .false._1 /)
  larr = (/ .true._8, .false._2 /)
  larr = (/ .true._8, .false._4 /)

  ut   = (/ usertype(20,4)(1), usertypeB(20,4)(2) /)
  utb  = (/ usertypeB(20,4)(1), usertype(20,4)(2) /)

end program acetnone01dl
