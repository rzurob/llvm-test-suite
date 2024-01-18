!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone02s
!*
!*  DATE                       : 2006-07-27
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : C494: values must have same type and values if no type spec
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array constructor, c494, square brackets
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  "C494 (R466) If type-spec is omitted, each ac-value expression in the array-
!*               constructor shall have the same type and kind type parameters."
!*
!*  Verify that using only the same types in the AC is permitted.
!*  Like acetnone02.f, but using square brackets.
!*
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

program acetnone02s

  use mod
  implicit none

  call poly([ -1, 0, 1 ])
  call poly([ -1_1, 0_1, 1_1 ])
  call poly([ -1_2, 0_2, 1_2 ])
  call poly([ -1_4, 0_4, 1_4 ])
  call poly([ -1_8, 0_8, 1_8 ])

  call poly([ -1.0, 0.0, 1.0 ])
  call poly([ -1.0_4,  0.0_4,  1.0_4 ])
  call poly([ -1.0_8,  0.0_8,  1.0_8 ])
  call poly([ -1.0_16, 0.0_16, 1.0_16 ])

  call poly([ (-1.0,0.0),      (0.0,1.0),      (1.0,1.0) ])
  call poly([ (-1.0_4,0.0_4),  (0.0_4,1.0_4),  (1.0_4,1.0_4) ])
  call poly([ (-1.0_8,0.0_8),  (0.0_8,1.0_8),  (1.0_8,1.0_8) ])
  call poly([ (-1.0_16,0.0_16),(0.0_16,1.0_16),(1.0_16,1.0_16)])

  call poly([ .true., .false. ])
  call poly([ .true._1, .false._1 ])
  call poly([ .true._2, .false._2 ])
  call poly([ .true._4, .false._4 ])
  call poly([ .true._8, .false._8 ])

  call poly([ 'a', 'b', 'c' ])
  call poly([ 1_'d', 1_'e', 1_'f' ])
  call poly([ 'gh', 'hi', 'ij' ])

  call poly([ usertype(1), usertype(2), usertype(3) ])
  call poly([ usertypeB(1), usertypeB(2), usertypeB(3) ])

contains

  subroutine poly(arg)
    class(*) :: arg(:)
  end subroutine poly

end program acetnone02s
