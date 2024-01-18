! GM DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/ace/diag/types/intrinsics/acetint31cd.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint31cdk
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetint31cd
!*                               by David Forster)
!*  DATE                       : 2007-12-03 (original: 2006-08-30)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
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

module acetint31cdkmod

  implicit none

  type :: modtype(k1,l1)    ! (4,1)
     integer, kind :: k1
     integer, len  :: l1
     character(l1) :: myval = 'a'
     procedure (character), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     character function myabs(this)
       import modtype
       class(modtype(4,*)), intent(in) :: this
     end function myabs
  end interface

contains

  character function moduleFunc(a)
    character :: a
    moduleFunc = 'd'
  end function moduleFunc

  character function mytest(this)
    class(modtype(4,*)), intent(in) :: this
    mytest = this % myval
  end function mytest

end module acetint31cdkmod


program acetint31cdk

  use acetint31cdkmod
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
  type (modtype(4,1)) :: mt

  pif => moduleFunc
  array = (/ moduleFunc, pif /)
  print *, array
  array = (/ character:: moduleFunc, pif /)
  print *, array

  p => externFunc
  array = (/ externFunc, p /)
  print *, array
  array = (/ character:: externFunc, p /)
  print *, array

  pif => anotherFunc
  array = (/ anotherFunc, pif /)
  print *, array
  array = (/ character:: anotherFunc, pif /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  array = (/ mt % modp, p /)
  print *, array
  array = (/ character:: mt % modp, p /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  array = (/ mt % myp, p2 /)
  print *, array
  array = (/ character:: mt % myp, p2 /)
  print *, array

end program acetint31cdk

character function externFunc(a)
  character :: a
  externFunc = 'b'
end function externFunc

character function anotherFunc(a)
  character :: a
  anotherFunc = 'c'
end function anotherFunc
