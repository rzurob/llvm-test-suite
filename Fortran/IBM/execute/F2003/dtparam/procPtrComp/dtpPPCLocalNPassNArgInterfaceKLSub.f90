!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCLocalNPassNArgInterfaceKLSub
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to subroutine with no argument (esp. no passed-object dummy argument)
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalNPassDTPInterfaceKLSub (<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to subroutines which do not
!*  expect any argument (especially no passed-object dummy argument).
!*  Define a parameterised derived type with two procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.  The arg which is passed in has
!*  kind and len parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalNPassNArgInterfaceKLSubmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (s1), pointer, nopass :: p1 => null()
     procedure (s2), pointer, nopass :: p2 => null()
  end type dt

  abstract interface

     subroutine s1
     end subroutine s1

     subroutine s2
     end subroutine s2

  end interface


contains


  subroutine disp1
    print *, "in disp1"
  end subroutine disp1

  subroutine disp2
    print *, "in disp2"
  end subroutine disp2


  subroutine disp1a
    print *, "in disp1a"
  end subroutine disp1a

  subroutine disp2a
    print *, "in disp2a"
  end subroutine disp2a

end module dtpPPCLocalNPassNArgInterfaceKLSubmod


program dtpPPCLocalNPassNArgInterfaceKLSub

  use dtpPPCLocalNPassNArgInterfaceKLSubmod
  implicit none
  type(dt(1,0)) :: t1a, t1b
  type(dt(2,4)) :: t2a
  type(dt(1,3)) :: t1c
  type(dt(1,:)) :: t1p
  type(dt(4,:)) :: t4p
  target  :: t1c
  pointer :: t1p, t4p

  t1a = dt(1,0)("",127,disp1,disp2)
  t1b = dt(1,0)("",-127,disp1a,disp2a)
  t1c = dt(1,3)("ijk",1,disp1,disp2)
  t1p => t1c
  t2a = dt(2,4)("abcd",32000,disp1,disp2)

  call t1a % p1 ! in disp1
  call t1a % p2 ! in disp2
  print *

  call t1b % p1 ! in disp1a
  call t1b % p2 ! in disp2a
  print *

  call t1c % p1 ! in disp1
  call t1c % p2 ! in disp2
  print *

  call t1p % p1 ! in disp1
  call t1p % p2 ! in disp2
  print *

  call t2a % p1 ! in disp1
  call t2a % p2 ! in disp2
  print *

  t1a % p1 => disp1a
  t1a % p2 => disp2a

  t1b % p1 => disp1
  t1b % p2 => disp2

  t1c % p1 => disp1a
  t1c % p2 => disp2a

  t2a % p1 => disp1a
  t2a % p2 => disp2a

  allocate(t1p, source=dt(1,5)("opqrs",0,disp1a,disp2a))

  call t1a % p1 ! in disp1a
  call t1a % p2 ! in disp2a
  print *

  call t1b % p1 ! in disp1
  call t1b % p2 ! in disp2
  print *

  call t1c % p1 ! in disp1a
  call t1c % p2 ! in disp2a
  print *

  call t1p % p1 ! in disp1a
  call t1p % p2 ! in disp2a
  print *

  call t2a % p1 ! in disp1a
  call t2a % p2 ! in disp2a
  print *

  print *, "done"

end program dtpPPCLocalNPassNArgInterfaceKLSub
