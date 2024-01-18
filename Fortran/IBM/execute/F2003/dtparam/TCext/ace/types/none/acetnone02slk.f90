!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetnone02slk
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetnone02s
!*                               by David Forster)
!*  DATE                       : 2008-01-29 (original: 2006-07-27)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement C494: values
!*                               must have same type and values if no type spec
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array constructor, c494, square brackets
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  "C494 (R466) If type-spec is omitted, each ac-value expression in the array-
!*               constructor shall have the same type and kind type parameters."
!*
!*  Verify that using only the same types in the AC is permitted.
!*  Like acetnone02.f, but using square brackets.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  implicit none

  type usertype (lusertype_1) ! lusertype_1=6
     integer, len :: lusertype_1
     integer(4) :: val
  end type usertype

  type usertypeB (kusertypeB_1) ! kusertypeB_1=4
     integer, kind :: kusertypeB_1
     integer(kusertypeB_1) :: valB
  end type usertypeB

end module mod

program acetnone02slk

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

  call poly([ usertype(6)(1), usertype(6)(2), usertype(6)(3) ]) ! tcx: (6) ! tcx: (6) ! tcx: (6)
  call poly([ usertypeB(4)(1), usertypeB(4)(2), usertypeB(4)(3) ]) ! tcx: (4) ! tcx: (4) ! tcx: (4)

contains

  subroutine poly(arg)
    class(*) :: arg(:)
  end subroutine poly

end program acetnone02slk


! Extensions to introduce derived type parameters:
! type: usertype - added parameters (lusertype_1) to invoke with (6)/declare with (*) - 3 changes
! type: usertypeB - added parameters (kusertypeB_1) to invoke with (4)/declare with (4) - 3 changes
