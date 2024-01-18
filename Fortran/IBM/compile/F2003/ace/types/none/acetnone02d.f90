!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone02d
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
!*  Use a mixture of types in the AC; the compiler should flag these as errors.
!*  Keep it simple: pairs of mismatched types in single constructors, rather
!*  than many different types.  Also, verify that the ordering doesn't matter,
!*  simply by trying both orderings.
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

program acetnone02d

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

  call poly((/ usertype(1), usertypeB(2) /))
  call poly((/ usertypeB(1), usertype(2) /))

contains

  subroutine poly(arg)
    class(*) :: arg(:)
  end subroutine poly

end program acetnone02d
