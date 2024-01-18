!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-08-30
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers are not acceptable data (character)
!*
!*  REFERENCE                  : Feature Number 289053
!*
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
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint31cdmod

  implicit none

  type :: modtype
     character :: myval = 'a'
     procedure (character), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     character function myabs(this)
       import modtype
       class(modtype), intent(in) :: this
     end function myabs
  end interface

contains

  character function moduleFunc(a)
    character :: a
    moduleFunc = 'd'
  end function moduleFunc

  character function mytest(this)
    class(modtype), intent(in) :: this
    mytest = this % myval
  end function mytest

end module acetint31cdmod


program acetint31cd

  use acetint31cdmod
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
  type (modtype) :: mt

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

end program acetint31cd

character function externFunc(a)
  character :: a
  externFunc = 'b'
end function externFunc

character function anotherFunc(a)
  character :: a
  anotherFunc = 'c'
end function anotherFunc
