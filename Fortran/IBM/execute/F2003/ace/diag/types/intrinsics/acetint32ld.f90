!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint32ld
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers are not acceptable data - implied-do (logical)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : procedure pointer, logical
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test logicals.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint32ldmod

  implicit none
  
  type :: modtype
     logical :: myval = .true.
     procedure (logical), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     logical function myabs(this)
       import modtype
       class(modtype), intent(in) :: this
     end function myabs
  end interface

contains

  logical function moduleFunc(a)
    logical :: a
    moduleFunc = .false.
  end function moduleFunc

  logical function mytest(this)
    class(modtype), intent(in) :: this
    mytest = this % myval
  end function mytest

end module acetint32ldmod


program acetint32ld

  use acetint32ldmod
  implicit none
  interface interfaceFunc
     logical function anotherFunc(a)
       logical :: a
     end function anotherFunc
  end interface

  abstract interface
     logical function aFunc(a)
       logical :: a
     end function aFunc
  end interface

  procedure(logical) :: externFunc
  procedure(logical), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  logical :: array(2)
  type (modtype) :: mt
  integer :: i

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ logical:: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ logical:: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ logical:: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ logical:: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ logical:: ( mt % myp, p2, i=1,1) /)
  print *, array

end program acetint32ld

logical function externFunc(a)
  logical :: a
  externFunc = .true. .eqv. a
end function externFunc

logical function anotherFunc(a)
  logical :: a
  anotherFunc = .not. a
end function anotherFunc
