!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : acetint32r
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers can be invoked in implied-do for data (real)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : procedure pointer, real
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test reals.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint32rmod

  implicit none
  
  type :: modtype
     real :: myval = 3.141526535897932384626433832795
     procedure (real), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     real function myabs(this)
       import modtype
       class(modtype), intent(in) :: this
     end function myabs
  end interface

contains

  real function moduleFunc(a)
    real :: a
    moduleFunc = a ** 2
  end function moduleFunc

  real function mytest(this)
    class(modtype), intent(in) :: this
    mytest = this % myval - 2.7182818284590452353602874713527
  end function mytest

end module acetint32rmod


program acetint32r

  use acetint32rmod
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
  type (modtype) :: mt
  integer :: i

  pif => moduleFunc
  print *, moduleFunc(1.4142136), pif(1.4142136)
  array = (/  (moduleFunc(1.4142136), pif(1.4142136), i=1,1) /)
  print *, array
  array = (/ real:: ( moduleFunc(1.4142136), pif(1.4142136), i=1,1) /)
  print *, array

  p => externFunc
  print *, externFunc(1.1), p(1.1)
  array = (/  (externFunc(1.1), p(1.1), i=1,1) /)
  print *, array
  array = (/ real:: ( externFunc(1.1), p(1.1), i=1,1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc(1.1), pif(1.1)
  array = (/  (anotherFunc(1.1), pif(1.1), i=1,1) /)
  print *, array
  array = (/ real:: ( anotherFunc(1.1), pif(1.1), i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp(1.1), p(1.1)
  array = (/  (mt % modp(1.1), p(1.1), i=1,1) /)
  print *, array
  array = (/ real:: ( mt % modp(1.1), p(1.1), i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/  (mt % myp(), p2(mt), i=1,1) /)
  print *, array
  array = (/ real:: ( mt % myp(), p2(mt), i=1,1) /)
  print *, array

end program acetint32r

real function externFunc(a)
  real :: a
  externFunc = a + 10
end function externFunc

real function anotherFunc(a)
  real :: a
  anotherFunc = a * 10
end function anotherFunc
