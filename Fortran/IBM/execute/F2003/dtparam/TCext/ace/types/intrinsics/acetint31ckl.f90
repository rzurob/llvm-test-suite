!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-08-30)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement procedure
!*                               pointers can be invoked for data (character)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, character
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test characters.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint31cmod

  implicit none

  type :: modtype (kmodtype_1,lmodtype_1) ! kmodtype_1,lmodtype_1=1,1
     integer, kind :: kmodtype_1
     integer, len :: lmodtype_1
     character(kmodtype_1) :: myval = 'm'
     procedure (character(kmodtype_1)), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     character function myabs(this)
       import modtype
       class(modtype(1,*)), intent(in) :: this ! tcx: (1,*)
     end function myabs
  end interface

contains

  character function moduleFunc(a)
    character :: a
    moduleFunc = char(ichar(a) + 1)
  end function moduleFunc

  character function mytest(this)
    class(modtype(1,*)), intent(in) :: this ! tcx: (1,*)
    mytest = char(ichar(this % myval) - 1)
  end function mytest

end module acetint31cmod


program acetint31ckl

  use acetint31cmod
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
  type (modtype(1,1)) :: mt ! tcx: (1,1)

  pif => moduleFunc
  print *, moduleFunc('d'), pif('d')
  array = (/ moduleFunc('d'), pif('d') /)
  print *, array
  array = (/ character:: moduleFunc('d'), pif('d') /)
  print *, array

  p => externFunc
  print *, externFunc('r'), p('R')
  array = (/ externFunc('r'), p('R') /)
  print *, array
  array = (/ character:: externFunc('R'), p('r') /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc('E'), pif('e')
  array = (/ anotherFunc('E'), pif('e') /)
  print *, array
  array = (/ character:: anotherFunc('e'), pif('E') /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp('Q'), p('q')
  array = (/ mt % modp('Q'), p('q') /)
  print *, array
  array = (/ character:: mt % modp('q'), p('Q') /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/ mt % myp(), p2(mt) /)
  print *, array
  array = (/ character:: mt % myp(), p2(mt) /)
  print *, array

end program acetint31ckl

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


! Extensions to introduce derived type parameters:
! type: modtype - added parameters (kmodtype_1,lmodtype_1) to invoke with (1,1)/declare with (1,*) - 3 changes
