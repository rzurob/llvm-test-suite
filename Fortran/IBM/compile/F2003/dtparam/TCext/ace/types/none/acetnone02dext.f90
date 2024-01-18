! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/ace/types/none/acetnone02d.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetnone02dext
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-07-26)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
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
!*  constructor shall have the same type and kind type parameters."
!*
!*  Use a mixture of types in the AC; the compiler should flag these as errors.
!*  Keep it simple: pairs of mismatched types in single constructors, rather
!*  than many different types.  Also, verify that the ordering doesn't matter,
!*  simply by trying both orderings.
!*
!*  Changes: originally written 06-16, revised and renamed 07-26 - subroutines
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  implicit none

  type usertype(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: val
  end type usertype

  type usertypeB(k2)    ! (4)
     integer, kind :: k2
     integer(k2)   :: valB
  end type usertypeB

end module mod

program acetnone02dext

  use mod
  implicit none

  ! Depending on -qintsize, 0 and 0_4 may not have the same kind.
  call poly((/ 0, 0_1 /))
  call poly((/ 0, 0_2 /))
  call poly((/ 0, 0_4 /))
  call poly((/ 0, 0_8 /))

  call poly((/ 0_1, 0 /))
  call poly((/ 0_1, 0_2 /))
  call poly((/ 0_1, 0_4 /))
  call poly((/ 0_1, 0_8 /))

  call poly((/ 0_2, 0 /))
  call poly((/ 0_2, 0_1 /))
  call poly((/ 0_2, 0_4 /))
  call poly((/ 0_2, 0_8 /))

  call poly((/ 0_4, 0 /))
  call poly((/ 0_4, 0_1 /))
  call poly((/ 0_4, 0_2 /))
  call poly((/ 0_4, 0_8 /))

  call poly((/ 0_8, 0 /))
  call poly((/ 0_8, 0_1 /))
  call poly((/ 0_8, 0_2 /))
  call poly((/ 0_8, 0_4 /))


  ! Depending on -qrealsize, 0.0 and 0.0_4 may not have the same kind.
  call poly((/ 0.0, 0.0_4 /))
  call poly((/ 0.0, 0.0_8 /))
  call poly((/ 0.0, 0.0_16 /))

  call poly((/ 0.0_4, 0.0 /))
  call poly((/ 0.0_4, 0.0_8 /))
  call poly((/ 0.0_4, 0.0_16 /))

  call poly((/ 0.0_8, 0.0 /))
  call poly((/ 0.0_8, 0.0_4 /))
  call poly((/ 0.0_8, 0.0_16 /))

  call poly((/ 0.0_16, 0.0 /))
  call poly((/ 0.0_16, 0.0_4 /))
  call poly((/ 0.0_16, 0.0_8 /))


  ! Only check complex numbers where the real and imaginary parts match.
  call poly((/ (0.0,0.0), (1.0_4,1.0_4) /))
  call poly((/ (0.0,0.0), (1.0_8,1.0_8) /))
  call poly((/ (0.0,0.0), (1.0_16,1.0_16) /))

  call poly((/ (0.0_4,0.0_4), (1.0,1.0) /))
  call poly((/ (0.0_4,0.0_4), (1.0_8,1.0_8) /))
  call poly((/ (0.0_4,0.0_4), (1.0_16,1.0_16) /))

  call poly((/ (0.0_8,0.0_8), (1.0,1.0) /))
  call poly((/ (0.0_8,0.0_8), (1.0_4,1.0_4) /))
  call poly((/ (0.0_8,0.0_8), (1.0_16,1.0_16) /))

  call poly((/ (0.0_16,0.0_16), (1.0,1.0) /))
  call poly((/ (0.0_16,0.0_16), (1.0_4,1.0_4) /))
  call poly((/ (0.0_16,0.0_16), (1.0_8,1.0_8) /))

  call poly((/ .true.,   .false._1 /))
  call poly((/ .true.,   .false._2 /))
  call poly((/ .true.,   .false._4 /))
  call poly((/ .true.,   .false._8 /))

  call poly((/ .true._1, .false.   /))
  call poly((/ .true._1, .false._2 /))
  call poly((/ .true._1, .false._4 /))
  call poly((/ .true._1, .false._8 /))

  call poly((/ .true._2, .false.   /))
  call poly((/ .true._2, .false._1 /))
  call poly((/ .true._2, .false._4 /))
  call poly((/ .true._2, .false._8 /))

  call poly((/ .true._4, .false.   /))
  call poly((/ .true._4, .false._1 /))
  call poly((/ .true._4, .false._2 /))
  call poly((/ .true._4, .false._8 /))

  call poly((/ .true._8, .false.   /))
  call poly((/ .true._8, .false._1 /))
  call poly((/ .true._8, .false._2 /))
  call poly((/ .true._8, .false._4 /))

  call poly((/ usertype(4)(1), usertypeB(4)(2) /))
  call poly((/ usertypeB(4)(1), usertype(4)(2) /))

contains

  subroutine poly(arg)
    class(*) :: arg(:)
  end subroutine poly

end program acetnone02dext
