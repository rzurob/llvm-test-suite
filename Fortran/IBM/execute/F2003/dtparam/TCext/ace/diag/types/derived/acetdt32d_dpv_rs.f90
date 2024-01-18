! GM DTP extension using:
! ftcx_dtp -qnok -qnol -qdefaultpv -qreuse=self /tstdev/F2003/ace/diag/types/derived/acetdt32d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt32d_dpv_rs
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt32d
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

module acetdt32d_dpv_rsmod

  implicit none

  type :: dt(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: field1
     real(k1)      :: field2
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
     type(dt(4)) :: myval = dt(4)(-999,-999.999)
     procedure (type(dt(4))), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     type(dt(4)) function myabs(this)
       import modtype, dt
       class(modtype), intent(in) :: this
     end function myabs
  end interface

contains

  elemental logical function dteq(this,a)
    class (dt(4)), intent(in) :: this, a
    dteq = (this % field1 == a % field1) .and. (this % field2 == a % field2)
  end function dteq

  type(dt(4)) function dtplus(this,a)
    class (dt(4)), intent(in) :: this, a
    dtplus % field1 = this % field1 + a % field1
    dtplus % field2 = this % field2 + a % field2
  end function dtplus

  type(dt(4)) function dtminus(this,a)
    class (dt(4)), intent(in) :: this, a
    dtminus % field1 = this % field1 - a % field1
    dtminus % field2 = this % field2 - a % field2
  end function dtminus

  type(dt(4)) function dtmult(this,a)
    class (dt(4)), intent(in) :: this, a
    dtmult % field1 = this % field1 * a % field1
    dtmult % field2 = this % field2 * a % field2
  end function dtmult

  type(dt(4)) function dtexp(this,a)
    class (dt(4)), intent(in) :: this, a
    dtexp % field1 = this % field1 ** a % field1
    dtexp % field2 = this % field2 ** a % field2
  end function dtexp

  type(dt(4)) function moduleFunc(a)
    type(dt(4)) :: a
    moduleFunc = a ** dt(4)(2,2)
  end function moduleFunc

  type(dt(4)) function mytest(this)
    class(modtype), intent(in) :: this
    mytest = this % myval - dt(4)(10,10)
  end function mytest

end module acetdt32d_dpv_rsmod


program acetdt32d_dpv_rs

  use acetdt32d_dpv_rsmod
  implicit none
  interface interfaceFunc
     type(dt(4)) function anotherFunc(a)
       use acetdt32d_dpv_rsmod
       type(dt(4)) :: a
     end function anotherFunc
  end interface

  abstract interface
     type(dt(4)) function aFunc(a)
       use acetdt32d_dpv_rsmod
       type(dt(4)) :: a
     end function aFunc
  end interface

  procedure(type(dt(4))) :: externFunc
  procedure(type(dt(4))), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  type(dt(4)) :: array(2), instance1
  type (modtype) :: mt
  integer  :: i

  instance1 % field1 = 99
  instance1 % field2 = 1.2

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ dt(4):: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ dt(4):: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ dt(4):: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ dt(4):: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ dt(4):: ( mt % myp, p2, i=1,1) /)
  print *, array

  array = (/  (dtmult, instance1 % dtexp, i=1,1) /)
  print *, array
  array = (/ dt(4):: ( dtmult, instance1 % dtexp, i=1,1) /)
  print *, array

  ! Perhaps we should also test the operators (+,-,*,**,==) with ACs
  ! of the form (/(*,i=1,1)/), etc., but these have certainly been tested to
  ! death elsewhere.  At the outside, maybe (/dt::(*,i=1,1)/) should be tested,
  ! but we'll resist the urge.

end program acetdt32d_dpv_rs

type(dt(4)) function externFunc(a)
  use acetdt32d_dpv_rsmod
  type(dt(4)) :: a
  externFunc = a + dt(4)(10,10)
end function externFunc

type(dt(4)) function anotherFunc(a)
  use acetdt32d_dpv_rsmod
  type(dt(4)) :: a
  anotherFunc = a * dt(4)(10,10)
end function anotherFunc
