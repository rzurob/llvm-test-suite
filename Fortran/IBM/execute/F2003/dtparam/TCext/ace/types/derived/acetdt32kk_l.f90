!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt32kk_l
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt32
!*                               by David Forster)
!*  DATE                       : 2008-01-14 (original: 2006-08-30)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers can be invoked in implied-do for
!*                               data (derived type)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
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
!*  Procedures referenced by procedure pointers can be invoked to produce
!*  data for the array constructor.  Here we test derived types.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt32mod

  implicit none

  type :: dt (kdt_1,kdt_2) ! kdt_1,kdt_2=4,4
     integer, kind :: kdt_1,kdt_2
     integer(kdt_1) :: field1
     real(kdt_2) :: field2
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

  type :: modtype (kmodtype_1,kmodtype_2,lmodtype_1) ! kmodtype_1,kmodtype_2,lmodtype_1=4,4,7
     integer, kind :: kmodtype_1,kmodtype_2
     integer, len :: lmodtype_1
     type(dt(kmodtype_1,kmodtype_2)) :: myval = dt(kmodtype_1,kmodtype_2)(-999,-999.999) ! tcx: (kmodtype_1,kmodtype_2) ! tcx: (kmodtype_1,kmodtype_2)
     procedure (type(dt(kmodtype_1,kmodtype_2))), nopass, pointer :: modp ! tcx: (kmodtype_1,kmodtype_2)
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     type(dt(4,4)) function myabs(this) ! tcx: (4,4)
       import modtype, dt
       class(modtype(4,4,*)), intent(in) :: this ! tcx: (4,4,*)
     end function myabs
  end interface

contains

  elemental logical function dteq(this,a)
    class (dt(4,4)), intent(in) :: this, a ! tcx: (4,4)
    dteq = (this % field1 == a % field1) .and. (this % field2 == a % field2)
  end function dteq

  type(dt(4,4)) function dtplus(this,a) ! tcx: (4,4)
    class (dt(4,4)), intent(in) :: this, a ! tcx: (4,4)
    dtplus % field1 = this % field1 + a % field1
    dtplus % field2 = this % field2 + a % field2
  end function dtplus

  type(dt(4,4)) function dtminus(this,a) ! tcx: (4,4)
    class (dt(4,4)), intent(in) :: this, a ! tcx: (4,4)
    dtminus % field1 = this % field1 - a % field1
    dtminus % field2 = this % field2 - a % field2
  end function dtminus

  type(dt(4,4)) function dtmult(this,a) ! tcx: (4,4)
    class (dt(4,4)), intent(in) :: this, a ! tcx: (4,4)
    dtmult % field1 = this % field1 * a % field1
    dtmult % field2 = this % field2 * a % field2
  end function dtmult

  type(dt(4,4)) function dtexp(this,a) ! tcx: (4,4)
    class (dt(4,4)), intent(in) :: this, a ! tcx: (4,4)
    dtexp % field1 = this % field1 ** a % field1
    dtexp % field2 = this % field2 ** a % field2
  end function dtexp

  type(dt(4,4)) function moduleFunc(a) ! tcx: (4,4)
    type(dt(4,4)) :: a ! tcx: (4,4)
    moduleFunc = a ** dt(4,4)(2,2) ! tcx: (4,4)
  end function moduleFunc

  type(dt(4,4)) function mytest(this) ! tcx: (4,4)
    class(modtype(4,4,*)), intent(in) :: this ! tcx: (4,4,*)
    mytest = this % myval - dt(4,4)(10,10) ! tcx: (4,4)
  end function mytest

end module acetdt32mod


program acetdt32kk_l

  use acetdt32mod
  implicit none
  interface interfaceFunc
     type(dt(4,4)) function anotherFunc(a) ! tcx: (4,4)
       use acetdt32mod
       type(dt(4,4)) :: a ! tcx: (4,4)
     end function anotherFunc
  end interface

  abstract interface
     type(dt(4,4)) function aFunc(a) ! tcx: (4,4)
       use acetdt32mod
       type(dt(4,4)) :: a ! tcx: (4,4)
     end function aFunc
  end interface

  procedure(type(dt(4,4))) :: externFunc ! tcx: (4,4)
  procedure(type(dt(4,4))), pointer :: p ! tcx: (4,4)
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  type(dt(4,4)) :: array(2), instance1, instance2 ! tcx: (4,4)
  type (modtype(4,4,7)) :: mt ! tcx: (4,4,7)
  integer  :: i

  instance1 % field1 = 99
  instance1 % field2 = 1.2

  instance2 % field1 = 101
  instance2 % field2 = 2.1

  pif => moduleFunc
  print *, moduleFunc(instance2), pif(instance2)
  array = (/  (moduleFunc(instance2), pif(instance2), i=1,1) /)
  print *, array
  array = (/ dt(4,4):: ( moduleFunc(instance2), pif(instance2), i=1,1) /) ! tcx: (4,4)
  print *, array

  p => externFunc
  print *, externFunc(instance1), p(instance1)
  array = (/  (externFunc(instance1), p(instance1), i=1,1) /)
  print *, array
  array = (/ dt(4,4):: ( externFunc(instance1), p(instance1), i=1,1) /) ! tcx: (4,4)
  print *, array

  pif => anotherFunc
  print *, anotherFunc(instance1), pif(instance1)
  array = (/  (anotherFunc(instance1), pif(instance1), i=1,1) /)
  print *, array
  array = (/ dt(4,4):: ( anotherFunc(instance1), pif(instance1), i=1,1) /) ! tcx: (4,4)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp(instance1), p(instance1)
  array = (/  (mt % modp(instance1), p(instance1), i=1,1) /)
  print *, array
  array = (/ dt(4,4):: ( mt % modp(instance1), p(instance1), i=1,1) /) ! tcx: (4,4)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/  (mt % myp(), p2(mt), i=1,1) /)
  print *, array
  array = (/ dt(4,4):: ( mt % myp(), p2(mt), i=1,1) /) ! tcx: (4,4)
  print *, array

  instance2 % field1 = 3

  print *, instance1 * instance2, instance1 ** instance2
  array = (/  (instance1 * instance2, instance1 ** instance2, i=1,1) /)
  print *, array
  array = (/  (dtmult(instance1, instance2), instance1 % dtexp(instance2), i=1,1) /)
  print *, array

  array = (/ dt(4,4):: ( instance1 * instance2, instance1 ** instance2, i=1,1) /) ! tcx: (4,4)
  print *, array
  array = (/ dt(4,4):: ( dtmult(instance1, instance2), instance1 % dtexp(instance2), i=1,1) /) ! tcx: (4,4)
  print *, array

  print *, (/  (instance1 == instance2, instance2 == instance2, i=1,1) /)
  print *, (/ logical:: ( instance1 == instance2, instance2 == instance2, i=1,1) /)

end program acetdt32kk_l

type(dt(4,4)) function externFunc(a) ! tcx: (4,4)
  use acetdt32mod
  type(dt(4,4)) :: a ! tcx: (4,4)
  externFunc = a + dt(4,4)(10,10) ! tcx: (4,4)
end function externFunc

type(dt(4,4)) function anotherFunc(a) ! tcx: (4,4)
  use acetdt32mod
  type(dt(4,4)) :: a ! tcx: (4,4)
  anotherFunc = a * dt(4,4)(10,10) ! tcx: (4,4)
end function anotherFunc


! Extensions to introduce derived type parameters:
! type: dt - added parameters (kdt_1,kdt_2) to invoke with (4,4) / declare with (4,4) - 38 changes
! type: modtype - added parameters (kmodtype_1,kmodtype_2,lmodtype_1) to invoke with (4,4,7) / declare with (4,4,*) - 3 changes
