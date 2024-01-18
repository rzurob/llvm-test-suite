!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint31zdlk
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-03 (original: 2006-08-30)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers are not acceptable data (complex)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint31zdmod

  implicit none

  type :: modtype (lmodtype_1,kmodtype_1) ! lmodtype_1,kmodtype_1=2,4
     integer, len :: lmodtype_1
     integer, kind :: kmodtype_1
     complex(kmodtype_1) :: myval = 5
     procedure (complex(kmodtype_1)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     complex function myabs(this)
       import modtype
       class(modtype(*,4)), intent(in) :: this ! tcx: (*,4)
     end function myabs
  end interface

contains

  complex function moduleFunc(a)
    complex :: a
    moduleFunc = a ** 2
  end function moduleFunc

  complex function mytest(this)
    class(modtype(*,4)), intent(in) :: this ! tcx: (*,4)
    mytest = this % myval - 10
  end function mytest

end module acetint31zdmod


program acetint31zdlk

  use acetint31zdmod
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
  type (modtype(2,4)) :: mt ! tcx: (2,4)

  pif => moduleFunc
  array = (/ moduleFunc, pif /)
  print *, array
  array = (/ complex:: moduleFunc, pif /)
  print *, array

  p => externFunc
  array = (/ externFunc, p /)
  print *, array
  array = (/ complex:: externFunc, p /)
  print *, array

  pif => anotherFunc
  array = (/ anotherFunc, pif /)
  print *, array
  array = (/ complex:: anotherFunc, pif /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/ mt % modp, p /)
  print *, array
  array = (/ complex:: mt % modp, p /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/ mt % myp, p2 /)
  print *, array
  array = (/ complex:: mt % myp, p2 /)
  print *, array

end program acetint31zdlk

complex function externFunc(a)
  complex :: a
  externFunc = a + 10
end function externFunc

complex function anotherFunc(a)
  complex :: a
  anotherFunc = a * 10
end function anotherFunc


! Extensions to introduce derived type parameters:
! type: modtype - added parameters (lmodtype_1,kmodtype_1) to invoke with (2,4)/declare with (*,4) - 3 changes
