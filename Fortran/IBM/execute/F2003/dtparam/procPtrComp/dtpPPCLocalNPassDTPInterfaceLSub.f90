!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCLocalNPassDTPInterfaceLSub
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to subroutine without passed-object dummy argument, DTP arg w/ len parameter
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
!*  expect a passed-object dummy argument.
!*  Define a parameterised derived type with two procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.  The arg which is passed in has
!*  a len parameter.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalNPassDTPInterfaceLSubmod

  implicit none

  type dt (l)
     integer, len :: l
     character(l) :: chval
     integer      :: ival
     procedure (s1), pointer, nopass :: p1 => null()
  end type dt

  abstract interface

     subroutine s1(a1)
       import :: dt
       class(dt(*)), intent(in) :: a1
     end subroutine s1

  end interface


contains


  subroutine disp1(this)
    class(dt(*)), intent(in) :: this
    print *, this%ival, kind(this%ival), len(this%chval), this%l, ">", this%chval, "< in disp1"
  end subroutine disp1

  subroutine disp1a(this)
    class(dt(*)), intent(in) :: this
    print *, this%ival, kind(this%ival), len(this%chval), this%l, ">", this%chval(1:this%l), "< in disp1a"
  end subroutine disp1a

end module dtpPPCLocalNPassDTPInterfaceLSubmod


program dtpPPCLocalNPassDTPInterfaceLSub

  use dtpPPCLocalNPassDTPInterfaceLSubmod
  implicit none
  type(dt(0)) :: t0a, t0b
  type(dt(4)) :: t4a, t4b
  type(dt(3)) :: t3c, t3d
  type(dt(:)) :: tp
  target  :: t3c
  pointer :: tp

  t0a = dt(0)("",127,disp1)
  t0b = dt(0)("",-127,disp1a)
  t4a = dt(4)("abcd",32000,disp1)
  t4b = dt(4)("efgh",-12345,disp1a)
  t3c = dt(3)("ijk",1,disp1)
  t3d = dt(3)("lmn",-1,disp1a)
  tp => t3c

  call t0a % p1(t0a) ! 127 4 0 0 >< in disp1
  call t0a % p1(t0b) ! -127 4 0 0 >< in disp1
  call t0a % p1(t3c) ! 1 4 3 3 >ijk< in disp1
  call t0a % p1(t3d) ! -1 4 3 3 >lmn< in disp1
  call t0a % p1(tp)  ! 1 4 3 3 >ijk< in disp1
  call t0a % p1(t4a) ! 32000 4 4 4 >abcd< in disp1
  call t0a % p1(t4b) ! -12345 4 4 4 >efgh< in disp1

  call t0b % p1(t0a) ! 127 4 0 0 >< in disp1a
  call t0b % p1(t0b) ! -127 4 0 0 >< in disp1a
  call t0b % p1(t3c) ! 1 4 3 3 >ijk< in disp1a
  call t0b % p1(t3d) ! -1 4 3 3 >lmn< in disp1a
  call t0b % p1(tp)  ! 1 4 3 3 >ijk< in disp1a
  call t0b % p1(t4a) ! 32000 4 4 4 >abcd< in disp1a
  call t0b % p1(t4b) ! -12345 4 4 4 >efgh< in disp1a

  call t4a % p1(t0a) ! 127 4 0 0 >< in disp1
  call t4a % p1(t0b) ! -127 4 0 0 >< in disp1
  call t4a % p1(t3c) ! 1 4 3 3 >ijk< in disp1
  call t4a % p1(t3d) ! -1 4 3 3 >lmn< in disp1
  call t4a % p1(tp)  ! 1 4 3 3 >ijk< in disp1
  call t4a % p1(t4a) ! 32000 4 4 4 >abcd< in disp1
  call t4a % p1(t4b) ! -12345 4 4 4 >efgh< in disp1

  call t4b % p1(t0a) ! 127 4 0 0 >< in disp1a
  call t4b % p1(t0b) ! -127 4 0 0 >< in disp1a
  call t4b % p1(t3c) ! 1 4 3 3 >ijk< in disp1a
  call t4b % p1(t3d) ! -1 4 3 3 >lmn< in disp1a
  call t4b % p1(tp)  ! 1 4 3 3 >ijk< in disp1a
  call t4b % p1(t4a) ! 32000 4 4 4 >abcd< in disp1a
  call t4b % p1(t4b) ! -12345 4 4 4 >efgh< in disp1a

  call t3c % p1(t0a) ! 127 4 0 0 >< in disp1
  call t3c % p1(t0b) ! -127 4 0 0 >< in disp1
  call t3c % p1(t3c) ! 1 4 3 3 >ijk< in disp1
  call t3c % p1(t3d) ! -1 4 3 3 >lmn< in disp1
  call t3c % p1(tp)  ! 1 4 3 3 >ijk< in disp1
  call t3c % p1(t4a) ! 32000 4 4 4 >abcd< in disp1
  call t3c % p1(t4b) ! -12345 4 4 4 >efgh< in disp1

  call t3d % p1(t0a) ! 127 4 0 0 >< in disp1a
  call t3d % p1(t0b) ! -127 4 0 0 >< in disp1a
  call t3d % p1(t3c) ! 1 4 3 3 >ijk< in disp1a
  call t3d % p1(t3d) ! -1 4 3 3 >lmn< in disp1a
  call t3d % p1(tp)  ! 1 4 3 3 >ijk< in disp1a
  call t3d % p1(t4a) ! 32000 4 4 4 >abcd< in disp1a
  call t3d % p1(t4b) ! -12345 4 4 4 >efgh< in disp1a

  call tp % p1(t0a) ! 127 4 0 0 >< in disp1
  call tp % p1(t0b) ! -127 4 0 0 >< in disp1
  call tp % p1(t3c) ! 1 4 3 3 >ijk< in disp1
  call tp % p1(t3d) ! -1 4 3 3 >lmn< in disp1
  call tp % p1(tp)  ! 1 4 3 3 >ijk< in disp1
  call tp % p1(t4a) ! 32000 4 4 4 >abcd< in disp1
  call tp % p1(t4b) ! -12345 4 4 4 >efgh< in disp1

  t0a % p1 => disp1a
  t0a % p1 => disp1a

  t0b % p1 => disp1
  t0b % p1 => disp1

  t3c % p1 => disp1a
  t3c % p1 => disp1a

  t3d % p1 => disp1
  t3d % p1 => disp1

  t4a % p1 => disp1a
  t4a % p1 => disp1a

  t4b % p1 => disp1
  t4b % p1 => disp1

  allocate(tp, source=dt(5)("opqrs",0,disp1a))

  call t0a % p1(t0a) ! 127 4 0 0 >< in disp1a
  call t0a % p1(t0b) ! -127 4 0 0 >< in disp1a
  call t0a % p1(t3c) ! 1 4 3 3 >ijk< in disp1a
  call t0a % p1(t3d) ! -1 4 3 3 >lmn< in disp1a
  call t0a % p1(tp)  ! 0 4 5 5 >opqrs< in disp1a
  call t0a % p1(t4a) ! 32000 4 4 4 >abcd< in disp1a
  call t0a % p1(t4b) ! -12345 4 4 4 >efgh< in disp1a

  call t0b % p1(t0a) ! 127 4 0 0 >< in disp1
  call t0b % p1(t0b) ! -127 4 0 0 >< in disp1
  call t0b % p1(t3c) ! 1 4 3 3 >ijk< in disp1
  call t0b % p1(t3d) ! -1 4 3 3 >lmn< in disp1
  call t0b % p1(tp)  ! 0 4 5 5 >opqrs< in disp1
  call t0b % p1(t4a) ! 32000 4 4 4 >abcd< in disp1
  call t0b % p1(t4b) ! -12345 4 4 4 >efgh< in disp1

  call t4a % p1(t0a) ! 127 4 0 0 >< in disp1a
  call t4a % p1(t0b) ! -127 4 0 0 >< in disp1a
  call t4a % p1(t3c) ! 1 4 3 3 >ijk< in disp1a
  call t4a % p1(t3d) ! -1 4 3 3 >lmn< in disp1a
  call t4a % p1(tp)  ! 0 4 5 5 >opqrs< in disp1a
  call t4a % p1(t4a) ! 32000 4 4 4 >abcd< in disp1a
  call t4a % p1(t4b) ! -12345 4 4 4 >efgh< in disp1a

  call t4b % p1(t0a) ! 127 4 0 0 >< in disp1
  call t4b % p1(t0b) ! -127 4 0 0 >< in disp1
  call t4b % p1(t3c) ! 1 4 3 3 >ijk< in disp1
  call t4b % p1(t3d) ! -1 4 3 3 >lmn< in disp1
  call t4b % p1(tp)  ! 0 4 5 5 >opqrs< in disp1
  call t4b % p1(t4a) ! 32000 4 4 4 >abcd< in disp1
  call t4b % p1(t4b) ! -12345 4 4 4 >efgh< in disp1

  call t3c % p1(t0a) ! 127 4 0 0 >< in disp1a
  call t3c % p1(t0b) ! -127 4 0 0 >< in disp1a
  call t3c % p1(t3c) ! 1 4 3 3 >ijk< in disp1a
  call t3c % p1(t3d) ! -1 4 3 3 >lmn< in disp1a
  call t3c % p1(tp)  ! 0 4 5 5 >opqrs< in disp1a
  call t3c % p1(t4a) ! 32000 4 4 4 >abcd< in disp1a
  call t3c % p1(t4b) ! -12345 4 4 4 >efgh< in disp1a

  call t3d % p1(t0a) ! 127 4 0 0 >< in disp1
  call t3d % p1(t0b) ! -127 4 0 0 >< in disp1
  call t3d % p1(t3c) ! 1 4 3 3 >ijk< in disp1
  call t3d % p1(t3d) ! -1 4 3 3 >lmn< in disp1
  call t3d % p1(tp)  ! 0 4 5 5 >opqrs< in disp1
  call t3d % p1(t4a) ! 32000 4 4 4 >abcd< in disp1
  call t3d % p1(t4b) ! -12345 4 4 4 >efgh< in disp1

  call tp % p1(t0a)  ! 127 4 0 0 >< in disp1a
  call tp % p1(t0b)  ! -127 4 0 0 >< in disp1a
  call tp % p1(t3c)  ! 1 4 3 3 >ijk< in disp1a
  call tp % p1(t3d)  ! -1 4 3 3 >lmn< in disp1a
  call tp % p1(tp)   ! 0 4 5 5 >opqrs< in disp1a
  call tp % p1(t4a)  ! 32000 4 4 4 >abcd< in disp1a
  call tp % p1(t4b)  ! -12345 4 4 4 >efgh< in disp1a

  allocate(tp, source=dt(5)("opqrs",123454321,disp1))
  call tp % p1(tp) ! 123454321 4 5 5 >pqr< in disp1
  call tp % p1(dt(3)("tuv", 543212345, disp1a)) ! 543212345 4 3 3 >u< in disp1

  print *, "done"

end program dtpPPCLocalNPassDTPInterfaceLSub
