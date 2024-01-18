!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint31rd
!*
!*  DATE                       : 2006-08-30
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers are not acceptable data (real)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test reals.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint31rdmod

  implicit none

  type :: modtype
     real :: myval = 5
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
    mytest = this % myval - 10
  end function mytest

end module acetint31rdmod


program acetint31rd

  use acetint31rdmod
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

  pif => moduleFunc
  array = (/ moduleFunc, pif /)
  print *, array
  array = (/ real:: moduleFunc, pif /)
  print *, array

  p => externFunc
  array = (/ externFunc, p /)
  print *, array
  array = (/ real:: externFunc, p /)
  print *, array

  pif => anotherFunc
  array = (/ anotherFunc, pif /)
  print *, array
  array = (/ real:: anotherFunc, pif /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/ mt % modp, p /)
  print *, array
  array = (/ real:: mt % modp, p /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/ mt % myp, p2 /)
  print *, array
  array = (/ real:: mt % myp, p2 /)
  print *, array

end program acetint31rd

real function externFunc(a)
  real :: a
  externFunc = a + 10
end function externFunc

real function anotherFunc(a)
  real :: a
  anotherFunc = a * 10
end function anotherFunc
