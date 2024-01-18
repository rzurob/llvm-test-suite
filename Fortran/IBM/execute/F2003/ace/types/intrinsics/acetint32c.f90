!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : acetint32c
!*
!*  DATE                       : 2006-08-30
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers can be invoked in implied-do for data (character)
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
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test characters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint32cmod

  implicit none

  type :: modtype
     character :: myval = 'm'
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
    moduleFunc = char(ichar(a) + 1)
  end function moduleFunc

  character function mytest(this)
    class(modtype), intent(in) :: this
    mytest = char(ichar(this % myval) - 1)
  end function mytest

end module acetint32cmod


program acetint32c

  use acetint32cmod
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
  integer :: i

  pif => moduleFunc
  print *, moduleFunc('d'), pif('d')
  array = (/  (moduleFunc('d'), pif('d'), i=1,1) /)
  print *, array
  array = (/ character:: ( moduleFunc('d'), pif('d'), i=1,1) /)
  print *, array

  p => externFunc
  print *, externFunc('r'), p('R')
  array = (/  (externFunc('r'), p('R'), i=1,1) /)
  print *, array
  array = (/ character:: ( externFunc('R'), p('r'), i=1,1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc('E'), pif('e')
  array = (/  (anotherFunc('E'), pif('e'), i=1,1) /)
  print *, array
  array = (/ character:: ( anotherFunc('e'), pif('E'), i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp('Q'), p('q')
  array = (/  (mt % modp('Q'), p('q'), i=1,1) /)
  print *, array
  array = (/ character:: ( mt % modp('q'), p('Q'), i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/  (mt % myp(), p2(mt), i=1,1) /)
  print *, array
  array = (/ character:: ( mt % myp(), p2(mt), i=1,1) /)
  print *, array

end program acetint32c

character function externFunc(a) ! upcases
  character :: a
  integer   :: code, diff
  code = iachar(a)
  externFunc = a
  if ( code >= iachar('a') ) then
     diff = iachar('a') - iachar('A')
     externFunc = achar(code - diff)
  endif
end function externFunc

character function anotherFunc(a) ! downcases
  character :: a
  integer   :: code, diff
  code = iachar(a)
  anotherFunc = a
  if ( code <= iachar('Z') ) then
     diff = iachar('z') - iachar('Z')
     anotherFunc = achar(code + diff)
  endif
end function anotherFunc
