!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : acetint15d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-06-08 (YYYY-MM-DD)
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : generics in AC via interface
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  The flip side of acetint11: Verify that incorrect combinations of
!*  types produced through generic functions are rejected.  Here, we
!*  test characters.
!*
!*  Change History:
!*  20060922 dforster The test case as originally conceived was invalid
!*                    because it required an automatic length field,
!*                    i.e., an implicit interface, and we needed to
!*                    have an explicit interface.  By using a single
!*                    parameter / two parameter pair (genCharacter1
!*                    and genCharacter2) instead, we have that.
!*
!* ====================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module mod

  interface varying
     module procedure genInteger8
     module procedure genInteger4
     module procedure genInteger2
     module procedure genInteger1

     module procedure genReal16
     module procedure genReal8
     module procedure genReal4

     module procedure genComplex16
     module procedure genComplex8
     module procedure genComplex4

     module procedure genLogical1
     module procedure genLogical2
     module procedure genLogical4
     module procedure genLogical8

     module procedure genCharacter1
     module procedure genCharacter2

  end interface

contains

  integer(1) function genInteger1(arg)
    integer(1) :: arg
    genInteger1 = arg * 10
  end function genInteger1

  integer(2) function genInteger2(arg)
    integer(2) :: arg
    genInteger2 = arg * 20
  end function genInteger2

  integer(4) function genInteger4(arg)
    integer(4) :: arg
    genInteger4 = arg * 40
  end function genInteger4

  integer(8) function genInteger8(arg)
    integer(8) :: arg
    genInteger8 = arg * 80
  end function genInteger8


  character(1) function genCharacter1(a1)
    character(*) :: a1
    genCharacter1 = a1
  end function genCharacter1

  character(2) function genCharacter2(a1,a2)
    character(*) :: a1, a2
    genCharacter2 = a1 // a2
  end function genCharacter2


  logical(1) function genLogical1(arg)
    logical(1) :: arg
    genLogical1 = .not. arg
  end function genLogical1

  logical(2) function genLogical2(arg)
    logical(2) :: arg
    genLogical2 = .not. arg
  end function genLogical2

  logical(4) function genLogical4(arg)
    logical(4) :: arg
    genLogical4 = .not. arg
  end function genLogical4

  logical(8) function genLogical8(arg)
    logical(8) :: arg
    genLogical8 = .not. arg
  end function genLogical8


  complex(4) function genComplex4(arg)
    complex(4) :: arg
    genComplex4 = arg + 10
  end function genComplex4

  complex(8) function genComplex8(arg)
    complex(8) :: arg
    genComplex8 = arg + (0,10)
  end function genComplex8

  complex(16) function genComplex16(arg)
    complex(16) :: arg
    genComplex16 = arg + (100,100)
  end function genComplex16


  real(4) function genReal4(arg)
    real(4) :: arg
    genReal4 = arg / 10
  end function genReal4

  real(8) function genReal8(arg)
    real(8) :: arg
    genReal8 = arg / 100
  end function genReal8

  real(16) function genReal16(arg)
    real(16) :: arg
    genReal16 = arg / 1000
  end function genReal16

end module mod

program acetint15d

  use mod
  implicit none

  character :: charr(4)

  ! Different types of character, determined by length:
  charr = (/varying('a'), varying('b','b'), varying('bbb','aa'), varying('')/)

  ! We shouldn't actually get here (the above should produce a severe compile error):
  if (any(charr /= (/'a', 'b', 'b', ' '/))) then
     print *, "Expecting (/'a', 'b', 'b', ' '/), but got: ", charr
     error stop 1_4
  end if

  ! Different types of character, determined by kind:
  charr = (/varying('a'), varying(2_'b'), varying(2_'b'), varying('')/)

  ! The above should produce a non-fatal compile error, forcing use of default kind:
  if (any(charr /= (/'a', 'b', 'b', ' '/))) then
     print *, "Expecting (/'a', 'b', 'b', ' '/), but got: ", charr
     error stop 2_4
  end if

end program acetint15d
