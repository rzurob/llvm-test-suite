!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-03 (original: 2006-08-30)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers are not acceptable data (integer)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint31idmod

  implicit none

  type :: modtype (kmodtype_1,lmodtype_1) ! kmodtype_1,lmodtype_1=4,3
     integer, kind :: kmodtype_1
     integer, len :: lmodtype_1
     integer(kmodtype_1) :: myval = 5
     procedure (integer(kmodtype_1)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     integer function myabs(this)
       import modtype
       class(modtype(4,*)), intent(in) :: this ! tcx: (4,*)
     end function myabs
  end interface

contains

  integer function moduleFunc(a)
    integer :: a
    moduleFunc = a ** 2
  end function moduleFunc

  integer function mytest(this)
    class(modtype(4,*)), intent(in) :: this ! tcx: (4,*)
    mytest = this % myval - 10
  end function mytest

end module acetint31idmod


program acetint31idkl

  use acetint31idmod
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
  type (modtype(4,3)) :: mt ! tcx: (4,3)

  pif => moduleFunc
  array = (/ moduleFunc, pif /)
  print *, array
  array = (/ integer:: moduleFunc, pif /)
  print *, array

  p => externFunc
  array = (/ externFunc, p /)
  print *, array
  array = (/ integer:: externFunc, p /)
  print *, array

  pif => anotherFunc
  array = (/ anotherFunc, pif /)
  print *, array
  array = (/ integer:: anotherFunc, pif /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/ mt % modp, p /)
  print *, array
  array = (/ integer:: mt % modp, p /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/ mt % myp, p2 /)
  print *, array
  array = (/ integer:: mt % myp, p2 /)
  print *, array

end program acetint31idkl

integer function externFunc(a)
  integer :: a
  externFunc = a + 10
end function externFunc

integer function anotherFunc(a)
  integer :: a
  anotherFunc = a * 10
end function anotherFunc


! Extensions to introduce derived type parameters:
! type: modtype - added parameters (kmodtype_1,lmodtype_1) to invoke with (4,3)/declare with (4,*) - 3 changes
