!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint31ldk
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetint31ld
!*                               by David Forster)
!*  DATE                       : 2007-12-03 (original: 2006-08-30)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers are not acceptable data (logical)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint31ldmod

  implicit none
  
  type :: modtype (kmodtype_1) ! kmodtype_1=4
     integer, kind :: kmodtype_1
     logical(kmodtype_1) :: myval = .true.
     procedure (logical(kmodtype_1)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     logical function myabs(this)
       import modtype
       class(modtype(4)), intent(in) :: this ! tcx: (4)
     end function myabs
  end interface

contains

  logical function moduleFunc(a)
    logical :: a
    moduleFunc = .false.
  end function moduleFunc

  logical function mytest(this)
    class(modtype(4)), intent(in) :: this ! tcx: (4)
    mytest = this % myval
  end function mytest

end module acetint31ldmod


program acetint31ldk

  use acetint31ldmod
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
  type (modtype(4)) :: mt ! tcx: (4)

  pif => moduleFunc
  array = (/ moduleFunc, pif /)
  print *, array
  array = (/ logical:: moduleFunc, pif /)
  print *, array

  p => externFunc
  array = (/ externFunc, p /)
  print *, array
  array = (/ logical:: externFunc, p /)
  print *, array

  pif => anotherFunc
  array = (/ anotherFunc, pif /)
  print *, array
  array = (/ logical:: anotherFunc, pif /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/ mt % modp, p /)
  print *, array
  array = (/ logical:: mt % modp, p /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/ mt % myp, p2 /)
  print *, array
  array = (/ logical:: mt % myp, p2 /)
  print *, array

end program acetint31ldk

logical function externFunc(a)
  logical :: a
  externFunc = .true. .eqv. a
end function externFunc

logical function anotherFunc(a)
  logical :: a
  anotherFunc = .not. a
end function anotherFunc


! Extensions to introduce derived type parameters:
! type: modtype - added parameters (kmodtype_1) to invoke with (4) / declare with (4) - 3 changes
