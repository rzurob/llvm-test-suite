!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-07 (from original 2006-11-24)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : generics in AC via interface
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This is based on acetint11, using the routines there with different
!*  intrinsic type specifiers.  From acetint11:
!*  "Generic functions can return different types of object, depending on which
!*  underlying function is actually invoked.  Which to invoke must actually be
!*  determinable at compile time, so the compiler should be able to correctly
!*  assemble the results, and weed out the cases which cannot work...."
!*  (Identical to acetint17, with the AC-IMP-DO's removed/converted.)
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

     module procedure genCharacter

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


  function genCharacter(arg,n)
    integer :: n
    character(*) :: arg
    character(n) :: genCharacter
    genCharacter = arg
  end function genCharacter


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

program acetint17a

  use mod
  implicit none

  integer :: iarr(3)
  real    :: rarr(3)
  double precision :: darr(3)
  complex :: zarr(1)
  logical :: larr(2)
  character :: charr(3)
  integer :: i

  iarr  = [integer:: varying(1), varying(2), varying(3)]
  print *, iarr
  print *, [integer:: varying(1), varying(2), varying(3)]

  rarr  = [real:: varying(1.0), varying(2.0), varying(3.0)]
  print *, rarr
  print *, [real:: varying(1.0), varying(2.0), varying(3.0)]

  darr  = [double precision:: varying(1.0d0), varying(2.0d0), varying(3.0d0)]
  print *, darr
  print *, [double precision:: varying(1.0d0), varying(2.0d0), varying(3.0d0)]

  zarr  = [complex:: varying((1.0,0.0))]
  print *, zarr
  print *, [complex:: varying((1.0,0.0))]

  larr  = [logical:: varying(.true.), varying(.false.)]
  print *, larr
  print *, [logical:: varying(.true.), varying(.false.)]

  charr = [character:: varying('a',1), varying('bbb',1), varying('',1)]
  print *, charr
  print *, [character:: varying('a',1), varying('bbb',1), varying('',1)]

  ! Now play around with types:
  print *, [integer:: varying(1), varying((2.1111d3,3.0d0)), varying(3.111d2)]
  print *, [real:: varying(1.0), varying(2), varying((3.0,5.1))]
  print *, [double precision:: varying(1), varying(2.2d0), varying((3.0d0,4.2d0))]
  print *, [complex(16):: varying((1.0,0.0)), varying(1), varying(1.3d0)]
  print *, [logical(8):: varying(.true.), varying(.false.)]
  print *, [character(3):: varying('a',1), varying('bbb',1), varying('',1)]
  print *, [character(3):: varying('a',3), varying('bbb',3), varying('wxyz',3)]

end program acetint17a