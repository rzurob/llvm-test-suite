!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2006-06-08 (YYYY-MM-DD)
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
!*  The flip side of acetint11: Verify that incorrect combinations of
!*  types produced through generic functions are rejected.  Here, we
!*  test complex numbers.
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

program acetint14d

  use mod
  implicit none

  complex :: zarr(4)

  ! different types of complex:
  zarr  = (/varying((1.0,0.1)), varying((4.0_4,0.4_4)), varying((8.0_8,0.8_8)), varying((16.0_16,0.16_16))/)

  ! Approximate comparison
  if (any(abs(zarr - (/(11.0,0.1), (14.0_4,0.4_4), (8.0_8,10.8_8), (116.0_16,100.16_16)/)) > 0.00001)) then
     print *, "Expecting (/(11.0,0.1), (14.0_4,0.4_4), (8.0_8,10.8_8), (116.0_16,100.16_16)/), but got: ", zarr
     error stop 1_4
  end if

  zarr  = (/varying(-1), varying(0), varying(2.0), varying((0.1,1.0))/)

  ! Approximate comparison
  if (any(abs(zarr - (/(-40.0,0.0), (0.0,0.0), (0.20,0.0), (10.1,1.0)/)) > 0.00001)) then
     print *, "Expecting (/(-10.0,0.0), (0.0,0.0), (0.20,0.0), (10.1,1.0)/), but got: ", zarr
     error stop 2_4
  end if


end program acetint14d
