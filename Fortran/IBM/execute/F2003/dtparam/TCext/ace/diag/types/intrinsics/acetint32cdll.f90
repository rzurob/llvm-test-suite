!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint32cdll
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetint32cd
!*                               by David Forster)
!*  DATE                       : 2007-12-03 (original: 2006-08-30)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers are not acceptable data --
!*                               implied-do (character)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : procedure pointer, character
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  AC-values can be any expression consistent with the type of the array to
!*  be constructed.  Procedure pointers are not in this set, when used as
!*  pointers.  Another test verifies that procedure pointers are allowed when
!*  used to invoke the function they reference.  Here we test characters.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint32cdmod

  implicit none
  
  type :: modtype (lmodtype_1,lmodtype_2) ! lmodtype_1,lmodtype_2=1,7
     integer, len :: lmodtype_1,lmodtype_2
     character(lmodtype_1) :: myval = 'a'
     procedure (character(lmodtype_1)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     character function myabs(this)
       import modtype
       class(modtype(*,*)), intent(in) :: this ! tcx: (*,*)
     end function myabs
  end interface

contains

  character function moduleFunc(a)
    character :: a
    moduleFunc = 'd'
  end function moduleFunc

  character function mytest(this)
    class(modtype(*,*)), intent(in) :: this ! tcx: (*,*)
    mytest = this % myval
  end function mytest

end module acetint32cdmod


program acetint32cdll

  use acetint32cdmod
  implicit none
  interface interfaceFunc
     character function anotherFunc(a)
       character :: a
     end function anotherFunc
  end interface

  abstract interface
     character function aFunc(a)
       character :: a
     end function aFunc
  end interface

  procedure(character) :: externFunc
  procedure(character), pointer :: p
  procedure(myabs), pointer :: p2
  procedure(aFunc), pointer :: pif
  character :: array(2)
  type (modtype(1,7)) :: mt ! tcx: (1,7)
  integer :: i

  pif => moduleFunc
  array = (/  (moduleFunc, pif, i=1,1) /)
  print *, array
  array = (/ character:: ( moduleFunc, pif, i=1,1) /)
  print *, array

  p => externFunc
  array = (/  (externFunc, p, i=1,1) /)
  print *, array
  array = (/ character:: ( externFunc, p, i=1,1) /)
  print *, array

  pif => anotherFunc
  array = (/  (anotherFunc, pif, i=1,1) /)
  print *, array
  array = (/ character:: ( anotherFunc, pif, i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/  (mt % modp, p, i=1,1) /)
  print *, array
  array = (/ character:: ( mt % modp, p, i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/  (mt % myp, p2, i=1,1) /)
  print *, array
  array = (/ character:: ( mt % myp, p2, i=1,1) /)
  print *, array

end program acetint32cdll

character function externFunc(a)
  character :: a
  externFunc = 'b'
end function externFunc

character function anotherFunc(a)
  character :: a
  anotherFunc = 'c'
end function anotherFunc


! Extensions to introduce derived type parameters:
! type: modtype - added parameters (lmodtype_1,lmodtype_2) to invoke with (1,7)/declare with (*,*) - 3 changes
