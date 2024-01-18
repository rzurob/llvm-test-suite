! GM DTP extension using:
! ftcx_dtp -qk -ql -qdefaultpv -qreuse=none /tstdev/F2003/ace/diag/types/derived/acetdt31d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt31dkl_dpv
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt31d
!*                               by David Forster)
!*  DATE                       : 2007-11-20 (original: 2006-08-30)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt31dkl_dpvmod

  implicit none

  type :: dt(l1,k1,k2)    ! (20,4,4)
     integer, kind :: k1,k2
     integer, len  :: l1
     integer(k1)   :: field1
     real(k2)      :: field2
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

  type :: modtype(k3,l2)    ! (4,20)
     integer, kind      :: k3
     integer, len       :: l2
     type(dt(l2,k3,k3)) :: myval! = dt(l2,k3,k3)(-999,-999.999)
     procedure (type(dt(20,4,4))), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     type(dt(20,4,4)) function myabs(this)
       import modtype, dt
       class(modtype(4,*)), intent(in) :: this
     end function myabs
  end interface

contains

  elemental logical function dteq(this,a)
    class (dt(*,4,4)), intent(in) :: this, a
    dteq = (this % field1 == a % field1) .and. (this % field2 == a % field2)
  end function dteq

  type(dt(20,4,4)) function dtplus(this,a)
    class (dt(*,4,4)), intent(in) :: this, a
    dtplus % field1 = this % field1 + a % field1
    dtplus % field2 = this % field2 + a % field2
  end function dtplus

  type(dt(20,4,4)) function dtminus(this,a)
    class (dt(*,4,4)), intent(in) :: this, a
    dtminus % field1 = this % field1 - a % field1
    dtminus % field2 = this % field2 - a % field2
  end function dtminus

  type(dt(20,4,4)) function dtmult(this,a)
    class (dt(*,4,4)), intent(in) :: this, a
    dtmult % field1 = this % field1 * a % field1
    dtmult % field2 = this % field2 * a % field2
  end function dtmult

  type(dt(20,4,4)) function dtexp(this,a)
    class (dt(*,4,4)), intent(in) :: this, a
    dtexp % field1 = this % field1 ** a % field1
    dtexp % field2 = this % field2 ** a % field2
  end function dtexp

  type(dt(20,4,4)) function moduleFunc(a)
    type(dt(*,4,4)) :: a
    moduleFunc = a ** dt(20,4,4)(2,2)
  end function moduleFunc

  type(dt(20,4,4)) function mytest(this)
    class(modtype(4,*)), intent(in) :: this
    mytest = this % myval - dt(20,4,4)(10,10)
  end function mytest

end module acetdt31dkl_dpvmod


program acetdt31dkl_dpv

  use acetdt31dkl_dpvmod
  implicit none
  interface interfaceFunc
     type(dt(20,4,4)) function anotherFunc(a)
       use acetdt31dkl_dpvmod
       type(dt(*,4,4)) :: a
     end function anotherFunc
  end interface

  abstract interface
     type(dt(20,4,4)) function aFunc(a)
       use acetdt31dkl_dpvmod
       type(dt(*,4,4)) :: a
     end function aFunc
  end interface

  procedure(type(dt(20,4,4))) :: externFunc
  procedure(type(dt(20,4,4))), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  type(dt(20,4,4)) :: array(2), instance1
  type (modtype(4,20)) :: mt

  instance1 % field1 = 99
  instance1 % field2 = 1.2

  pif => moduleFunc
  array = (/ moduleFunc, pif /)
  print *, array
  array = (/ dt(20,4,4):: moduleFunc, pif /)
  print *, array

  p => externFunc
  array = (/ externFunc, p /)
  print *, array
  array = (/ dt(20,4,4):: externFunc, p /)
  print *, array

  pif => anotherFunc
  array = (/ anotherFunc, pif /)
  print *, array
  array = (/ dt(20,4,4):: anotherFunc, pif /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/ mt % modp, p /)
  print *, array
  array = (/ dt(20,4,4):: mt % modp, p /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/ mt % myp, p2 /)
  print *, array
  array = (/ dt(20,4,4):: mt % myp, p2 /)
  print *, array

  array = (/ dtmult, instance1 % dtexp /)
  print *, array
  array = (/ dt(20,4,4):: dtmult, instance1 % dtexp /)
  print *, array

  ! Perhaps we should also test the operators (+,-,*,**,==) with ACs
  ! of the form (/ * /), etc., but these have certainly been tested to
  ! death elsewhere.  At the outside, maybe (/dt::*/) should be tested,
  ! but we'll resist the urge.

end program acetdt31dkl_dpv

type(dt(20,4,4)) function externFunc(a)
  use acetdt31dkl_dpvmod
  type(dt(*,4,4)) :: a
  externFunc = a + dt(20,4,4)(10,10)
end function externFunc

type(dt(20,4,4)) function anotherFunc(a)
  use acetdt31dkl_dpvmod
  type(dt(*,4,4)) :: a
  anotherFunc = a * dt(20,4,4)(10,10)
end function anotherFunc
