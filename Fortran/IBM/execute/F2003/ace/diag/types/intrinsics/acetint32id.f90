!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint32id
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers are not acceptable data - implied-do (integer)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : procedure pointer, integer
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test integers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint32idmod

  implicit none
  
  type :: modtype
     integer :: myval = 5
     procedure (integer), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     integer function myabs(this)
       import modtype
       class(modtype), intent(in) :: this
     end function myabs
  end interface

contains

  integer function moduleFunc(a)
    integer :: a
    moduleFunc = a ** 2
  end function moduleFunc

  integer function mytest(this)
    class(modtype), intent(in) :: this
    mytest = this % myval - 10
  end function mytest

end module acetint32idmod


program acetint32id

  use acetint32idmod
  implicit none
  interface interfaceFunc
     integer function anotherFunc(a)
       integer :: a
     end function anotherFunc
  end interface

  abstract interface
     integer function aFunc(a)
       integer :: a
     end function aFunc
  end interface

  procedure(integer) :: externFunc
  procedure(integer), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  integer :: array(2)
  type (modtype) :: mt
  integer :: i

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ integer:: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ integer:: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ integer:: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ integer:: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ integer:: ( mt % myp, p2, i=1,1) /)
  print *, array

end program acetint32id

integer function externFunc(a)
  integer :: a
  externFunc = a + 10
end function externFunc

integer function anotherFunc(a)
  integer :: a
  anotherFunc = a * 10
end function anotherFunc
