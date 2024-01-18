! GM DTP extension using:
! ftcx_dtp -qk -qnol -qdefaultpv -qreuse=none /tstdev/F2003/ace/types/derived/acetdt31.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt31k_dpv
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt31
!*                               by David Forster)
!*  DATE                       : 2008-01-14 (original: 2006-08-30)
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
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test derived types.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt31k_dpvmod

  implicit none

  type :: dt(k1,k2)    ! (4,4)
     integer, kind :: k1,k2
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

  type :: modtype(k3)    ! (4)
     integer, kind   :: k3
     type(dt(k3,k3)) :: myval = dt(k3,k3)(-999,-999.999)
     procedure (type(dt(4,4))), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     type(dt(4,4)) function myabs(this)
       import modtype, dt
       class(modtype(4)), intent(in) :: this
     end function myabs
  end interface

contains

  elemental logical function dteq(this,a)
    class (dt(4,4)), intent(in) :: this, a
    dteq = (this % field1 == a % field1) .and. (this % field2 == a % field2)
  end function dteq

  type(dt(4,4)) function dtplus(this,a)
    class (dt(4,4)), intent(in) :: this, a
    dtplus % field1 = this % field1 + a % field1
    dtplus % field2 = this % field2 + a % field2
  end function dtplus

  type(dt(4,4)) function dtminus(this,a)
    class (dt(4,4)), intent(in) :: this, a
    dtminus % field1 = this % field1 - a % field1
    dtminus % field2 = this % field2 - a % field2
  end function dtminus

  type(dt(4,4)) function dtmult(this,a)
    class (dt(4,4)), intent(in) :: this, a
    dtmult % field1 = this % field1 * a % field1
    dtmult % field2 = this % field2 * a % field2
  end function dtmult

  type(dt(4,4)) function dtexp(this,a)
    class (dt(4,4)), intent(in) :: this, a
    dtexp % field1 = this % field1 ** a % field1
    dtexp % field2 = this % field2 ** a % field2
  end function dtexp

  type(dt(4,4)) function moduleFunc(a)
    type(dt(4,4)) :: a
    moduleFunc = a ** dt(4,4)(2,2)
  end function moduleFunc

  type(dt(4,4)) function mytest(this)
    class(modtype(4)), intent(in) :: this
    mytest = this % myval - dt(4,4)(10,10)
  end function mytest

end module acetdt31k_dpvmod


program acetdt31k_dpv

  use acetdt31k_dpvmod
  implicit none
  interface interfaceFunc
     type(dt(4,4)) function anotherFunc(a)
       use acetdt31k_dpvmod
       type(dt(4,4)) :: a
     end function anotherFunc
  end interface

  abstract interface
     type(dt(4,4)) function aFunc(a)
       use acetdt31k_dpvmod
       type(dt(4,4)) :: a
     end function aFunc
  end interface

  procedure(type(dt(4,4))) :: externFunc
  procedure(type(dt(4,4))), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  type(dt(4,4)) :: array(2), instance1, instance2
  type (modtype(4)) :: mt

  instance1 % field1 = 99
  instance1 % field2 = 1.2

  instance2 % field1 = 101
  instance2 % field2 = 2.1

  pif => moduleFunc
  print *, moduleFunc(instance2), pif(instance2)
  array = (/ moduleFunc(instance2), pif(instance2) /)
  print *, array
  array = (/ dt(4,4):: moduleFunc(instance2), pif(instance2) /)
  print *, array

  p => externFunc
  print *, externFunc(instance1), p(instance1)
  array = (/ externFunc(instance1), p(instance1) /)
  print *, array
  array = (/ dt(4,4):: externFunc(instance1), p(instance1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc(instance1), pif(instance1)
  array = (/ anotherFunc(instance1), pif(instance1) /)
  print *, array
  array = (/ dt(4,4):: anotherFunc(instance1), pif(instance1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp(instance1), p(instance1)
  array = (/ mt % modp(instance1), p(instance1) /)
  print *, array
  array = (/ dt(4,4):: mt % modp(instance1), p(instance1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/ mt % myp(), p2(mt) /)
  print *, array
  array = (/ dt(4,4):: mt % myp(), p2(mt) /)
  print *, array

  instance2 % field1 = 3

  print *, instance1 * instance2, instance1 ** instance2
  array = (/ instance1 * instance2, instance1 ** instance2 /)
  print *, array
  array = (/ dtmult(instance1, instance2), instance1 % dtexp(instance2) /)
  print *, array

  array = (/ dt(4,4):: instance1 * instance2, instance1 ** instance2 /)
  print *, array
  array = (/ dt(4,4):: dtmult(instance1, instance2), instance1 % dtexp(instance2) /)
  print *, array

  print *, (/ instance1 == instance2, instance2 == instance2 /)
  print *, (/ logical:: instance1 == instance2, instance2 == instance2 /)

end program acetdt31k_dpv

type(dt(4,4)) function externFunc(a)
  use acetdt31k_dpvmod
  type(dt(4,4)) :: a
  externFunc = a + dt(4,4)(10,10)
end function externFunc

type(dt(4,4)) function anotherFunc(a)
  use acetdt31k_dpvmod
  type(dt(4,4)) :: a
  anotherFunc = a * dt(4,4)(10,10)
end function anotherFunc
