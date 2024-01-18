!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetdt32d
!*
!*  DATE                       : 2006-08-30
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers are not acceptable data - implied-do (derived type)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, derived type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test derived types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt32dmod

  implicit none

  type :: dt
     integer :: field1
     real :: field2
   contains
     procedure :: dteq
     procedure :: dtplus
     procedure :: dtminus
     procedure :: dtmult
     procedure :: dtexp
     generic :: operator(.eq.) => dteq
     generic :: operator(+) => dtplus
     generic :: operator(-) => dtminus
     generic :: operator(*) => dtmult
     generic :: operator(**) => dtexp
  end type dt

  type :: modtype
     type(dt) :: myval = dt(-999,-999.999)
     procedure (type(dt)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     type(dt) function myabs(this)
       import modtype, dt
       class(modtype), intent(in) :: this
     end function myabs
  end interface

contains

  elemental logical function dteq(this,a)
    class (dt), intent(in) :: this, a
    dteq = (this % field1 == a % field1) .and. (this % field2 == a % field2)
  end function dteq

  type(dt) function dtplus(this,a)
    class (dt), intent(in) :: this, a
    dtplus % field1 = this % field1 + a % field1
    dtplus % field2 = this % field2 + a % field2
  end function dtplus

  type(dt) function dtminus(this,a)
    class (dt), intent(in) :: this, a
    dtminus % field1 = this % field1 - a % field1
    dtminus % field2 = this % field2 - a % field2
  end function dtminus

  type(dt) function dtmult(this,a)
    class (dt), intent(in) :: this, a
    dtmult % field1 = this % field1 * a % field1
    dtmult % field2 = this % field2 * a % field2
  end function dtmult

  type(dt) function dtexp(this,a)
    class (dt), intent(in) :: this, a
    dtexp % field1 = this % field1 ** a % field1
    dtexp % field2 = this % field2 ** a % field2
  end function dtexp

  type(dt) function moduleFunc(a)
    type(dt) :: a
    moduleFunc = a ** dt(2,2)
  end function moduleFunc

  type(dt) function mytest(this)
    class(modtype), intent(in) :: this
    mytest = this % myval - dt(10,10)
  end function mytest

end module acetdt32dmod


program acetdt32d

  use acetdt32dmod
  implicit none
  interface interfaceFunc
     type(dt) function anotherFunc(a)
       use acetdt32dmod
       type(dt) :: a
     end function anotherFunc
  end interface

  abstract interface
     type(dt) function aFunc(a)
       use acetdt32dmod
       type(dt) :: a
     end function aFunc
  end interface

  procedure(type(dt)) :: externFunc
  procedure(type(dt)), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  type(dt) :: array(2), instance1
  type (modtype) :: mt
  integer  :: i

  instance1 % field1 = 99
  instance1 % field2 = 1.2

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ dt:: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ dt:: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ dt:: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ dt:: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ dt:: ( mt % myp, p2, i=1,1) /)
  print *, array

  array = (/  (dtmult, instance1 % dtexp, i=1,1) /)
  print *, array
  array = (/ dt:: ( dtmult, instance1 % dtexp, i=1,1) /)
  print *, array

  ! Perhaps we should also test the operators (+,-,*,**,==) with ACs
  ! of the form (/(*,i=1,1)/), etc., but these have certainly been tested to
  ! death elsewhere.  At the outside, maybe (/dt::(*,i=1,1)/) should be tested,
  ! but we'll resist the urge.

end program acetdt32d

type(dt) function externFunc(a)
  use acetdt32dmod
  type(dt) :: a
  externFunc = a + dt(10,10)
end function externFunc

type(dt) function anotherFunc(a)
  use acetdt32dmod
  type(dt) :: a
  anotherFunc = a * dt(10,10)
end function anotherFunc
