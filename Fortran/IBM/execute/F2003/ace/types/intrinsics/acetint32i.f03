!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2006-08-30
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers can be invoked in implied-do for data (integer)
!*
!*  REFERENCE                  : Feature Number 289053
!*
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

module acetint32imod

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

end module acetint32imod


program acetint32i

  use acetint32imod
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
  print *, moduleFunc(2), pif(2)
  array = (/  (moduleFunc(2), pif(2), i=1,1) /)
  print *, array
  array = (/ integer:: ( moduleFunc(2), pif(2), i=1,1) /)
  print *, array

  p => externFunc
  print *, externFunc(1), p(1)
  array = (/  (externFunc(1), p(1), i=1,1) /)
  print *, array
  array = (/ integer:: ( externFunc(1), p(1), i=1,1) /)
  print *, array

  pif => anotherFunc
  print *, anotherFunc(1), pif(1)
  array = (/  (anotherFunc(1), pif(1), i=1,1) /)
  print *, array
  array = (/ integer:: ( anotherFunc(1), pif(1), i=1,1) /)
  print *, array

  mt % modp => externFunc
  p => mt % modp
  print *, mt % modp(1), p(1)
  array = (/  (mt % modp(1), p(1), i=1,1) /)
  print *, array
  array = (/ integer:: ( mt % modp(1), p(1), i=1,1) /)
  print *, array

  mt % myp => mytest
  p2 => mt % myp
  print *, mt % myp(), p2(mt)
  array = (/  (mt % myp(), p2(mt), i=1,1) /)
  print *, array
  array = (/ integer:: ( mt % myp(), p2(mt), i=1,1) /)
  print *, array

end program acetint32i

integer function externFunc(a)
  integer :: a
  externFunc = a + 10
end function externFunc

integer function anotherFunc(a)
  integer :: a
  anotherFunc = a * 10
end function anotherFunc
