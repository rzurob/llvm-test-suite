!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-08-30
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers are not acceptable data - implied-do (complex)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, complex
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test complex numbers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint32zdmod

  implicit none

  type :: modtype
     complex :: myval = 5
     procedure (complex), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     complex function myabs(this)
       import modtype
       class(modtype), intent(in) :: this
     end function myabs
  end interface

contains

  complex function moduleFunc(a)
    complex :: a
    moduleFunc = a ** 2
  end function moduleFunc

  complex function mytest(this)
    class(modtype), intent(in) :: this
    mytest = this % myval - 10
  end function mytest

end module acetint32zdmod


program acetint32zd

  use acetint32zdmod
  implicit none
  interface interfaceFunc
     complex function anotherFunc(a)
       complex :: a
     end function anotherFunc
  end interface

  abstract interface
     complex function aFunc(a)
       complex :: a
     end function aFunc
  end interface

  procedure(complex) :: externFunc
  procedure(complex), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  complex :: array(2)
  type (modtype) :: mt
  integer :: i

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ complex:: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ complex:: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ complex:: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ complex:: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ complex:: ( mt % myp, p2, i=1,1) /)
  print *, array

end program acetint32zd

complex function externFunc(a)
  complex :: a
  externFunc = a + 10
end function externFunc

complex function anotherFunc(a)
  complex :: a
  anotherFunc = a * 10
end function anotherFunc
