!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2006-08-30
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers can be invoked in implied-do for data (logical)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : procedure pointer, logical
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Procedures referenced by procedure pointers can be invoked to produce data
!*  for the array constructor.  Here we test logicals.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint32lmod

  implicit none

  type :: modtype
     logical :: myval = .true.
     procedure (logical), nopass, pointer :: modp
     procedure (myabs), pass, pointer :: myp
   contains
     procedure :: mytest
  end type modtype

  abstract interface
     logical function myabs(this, b)
       import modtype
       class(modtype), intent(in) :: this
       logical :: b
     end function myabs
  end interface

contains

  logical function moduleFunc(a, b)
    logical :: a, b
    moduleFunc = a .eqv. b
  end function moduleFunc

  logical function mytest(this, b)
    class(modtype), intent(in) :: this
    logical :: b
    mytest = this % myval .neqv. b
  end function mytest

end module acetint32lmod


program acetint32l

  use acetint32lmod
  implicit none
  interface interfaceFunc
     logical function anotherFunc(a,b)
       logical :: a, b
     end function anotherFunc
  end interface

  abstract interface
     logical function aFunc(a,b)
       logical :: a, b
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
  print *, moduleFunc(.true.,.false.), pif(.true.,.true.)
  array = (/  (moduleFunc(.true.,.false.), pif(.true.,.true.), i=1,1) /)
  print *, array
  array = (/ logical:: ( moduleFunc(.true.,.false.), pif(.true.,.true.), i=1,1) /)
  print *, array

  p => externFunc
  print *, externFunc(.true.,.true.), p(.false.,.true.)
  array = (/  (externFunc(.true.,.true.), p(.false.,.true.), i=1,1) /)
  print *, array
  array = (/ logical:: ( externFunc(.true.,.true.), p(.false.,.true.), i=1,1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc(.false.,.true.), pif(.true.,.false.)
  array = (/  (anotherFunc(.false.,.true.), pif(.true.,.false.), i=1,1) /)
  print *, array
  array = (/ logical:: ( anotherFunc(.false.,.true.), pif(.true.,.false.), i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp(.false.,.false.), p(.true.,.true.)
  array = (/  (mt % modp(.false.,.false.), p(.true.,.true.), i=1,1) /)
  print *, array
  array = (/ logical:: ( mt % modp(.false.,.false.), p(.true.,.true.), i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(.false.), p2(mt,.true.)
  array = (/  (mt % myp(.false.), p2(mt,.true.), i=1,1) /)
  print *, array
  array = (/ logical:: ( mt % myp(.false.), p2(mt,.true.), i=1,1) /)
  print *, array

end program acetint32l

logical function externFunc(a,b)
  logical :: a, b
  externFunc = a .and. b
end function externFunc

logical function anotherFunc(a,b)
  logical :: a, b
  anotherFunc = a .or. b
end function anotherFunc
