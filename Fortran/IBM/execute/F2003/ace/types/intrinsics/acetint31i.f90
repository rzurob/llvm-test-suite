!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : acetint31i
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers can be invoked for data (integer)
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
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test integers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint31imod

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

end module acetint31imod


program acetint31i

  use acetint31imod
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

  pif => moduleFunc
  print *, moduleFunc(2), pif(2)
  array = (/ moduleFunc(2), pif(2) /)
  print *, array
  array = (/ integer:: moduleFunc(2), pif(2) /)
  print *, array

  p => externFunc
  print *, externFunc(1), p(1)
  array = (/ externFunc(1), p(1) /)
  print *, array
  array = (/ integer:: externFunc(1), p(1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc(1), pif(1)
  array = (/ anotherFunc(1), pif(1) /)
  print *, array
  array = (/ integer:: anotherFunc(1), pif(1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp(1), p(1)
  array = (/ mt % modp(1), p(1) /)
  print *, array
  array = (/ integer:: mt % modp(1), p(1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/ mt % myp(), p2(mt) /)
  print *, array
  array = (/ integer:: mt % myp(), p2(mt) /)
  print *, array

end program acetint31i

integer function externFunc(a)
  integer :: a
  externFunc = a + 10
end function externFunc

integer function anotherFunc(a)
  integer :: a
  anotherFunc = a * 10
end function anotherFunc
