!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-08-30)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers can be invoked for data (real)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test reals.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint31rmod

  implicit none

  type :: modtype (kmodtype_1,lmodtype_1) ! kmodtype_1,lmodtype_1=4,11
     integer, kind :: kmodtype_1
     integer, len :: lmodtype_1
     real(kmodtype_1) :: myval = 3.141526535897932384626433832795
     procedure (real(kmodtype_1)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     real function myabs(this)
       import modtype
       class(modtype(4,*)), intent(in) :: this ! tcx: (4,*)
     end function myabs
  end interface

contains

  real function moduleFunc(a)
    real :: a
    moduleFunc = a ** 2
  end function moduleFunc

  real function mytest(this)
    class(modtype(4,*)), intent(in) :: this ! tcx: (4,*)
    mytest = this % myval - 2.7182818284590452353602874713527
  end function mytest

end module acetint31rmod


program acetint31rkl

  use acetint31rmod
  implicit none
  interface interfaceFunc
     real function anotherFunc(a)
       real :: a
     end function anotherFunc
  end interface

  abstract interface
     real function aFunc(a)
       real :: a
     end function aFunc
  end interface

  procedure(real) :: externFunc
  procedure(real), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  real :: array(2)
  type (modtype(4,11)) :: mt ! tcx: (4,11)

  pif => moduleFunc
  print *, moduleFunc(1.4142136), pif(1.4142136)
  array = (/ moduleFunc(1.4142136), pif(1.4142136) /)
  print *, array
  array = (/ real:: moduleFunc(1.4142136), pif(1.4142136) /)
  print *, array

  p => externFunc
  print *, externFunc(1.1), p(1.1)
  array = (/ externFunc(1.1), p(1.1) /)
  print *, array
  array = (/ real:: externFunc(1.1), p(1.1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc(1.1), pif(1.1)
  array = (/ anotherFunc(1.1), pif(1.1) /)
  print *, array
  array = (/ real:: anotherFunc(1.1), pif(1.1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp(1.1), p(1.1)
  array = (/ mt % modp(1.1), p(1.1) /)
  print *, array
  array = (/ real:: mt % modp(1.1), p(1.1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/ mt % myp(), p2(mt) /)
  print *, array
  array = (/ real:: mt % myp(), p2(mt) /)
  print *, array

end program acetint31rkl

real function externFunc(a)
  real :: a
  externFunc = a + 10
end function externFunc

real function anotherFunc(a)
  real :: a
  anotherFunc = a * 10
end function anotherFunc


! Extensions to introduce derived type parameters:
! type: modtype - added parameters (kmodtype_1,lmodtype_1) to invoke with (4,11)/declare with (4,*) - 3 changes
