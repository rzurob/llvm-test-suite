!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCBasicNoPassSubroutine
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to subroutine without passed-object dummy argument
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
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
!*  Uses local variables to reference objects.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCBasicNoPassSubroutinemod

  implicit none

  type dt (k)
     integer, kind :: k
     integer(k) :: ival
     procedure (s1), pointer, nopass :: p1 => null()
     procedure (s2), pointer, nopass :: p2 => null()
  end type dt

  abstract interface

     subroutine s1(a1)
       import :: dt
       class(dt(1)), intent(in) :: a1
     end subroutine s1

     subroutine s2(a1)
       import :: dt
       class(dt(2)), intent(in) :: a1
     end subroutine s2

  end interface


contains


  subroutine disp1(this)
    class(dt(1)), intent(in) :: this
    print *, this % ival, "in disp1"
  end subroutine disp1

  subroutine disp2(this)
    class(dt(2)), intent(in) :: this
    print *, this % ival, "in disp2"
  end subroutine disp2


  subroutine disp1a(this)
    class(dt(1)), intent(in) :: this
    print *, this % ival, "in disp1a"
  end subroutine disp1a

  subroutine disp2a(this)
    class(dt(2)), intent(in) :: this
    print *, this % ival, "in disp2a"
  end subroutine disp2a

end module dtpPPCBasicNoPassSubroutinemod


program dtpPPCBasicNoPassSubroutine

  use dtpPPCBasicNoPassSubroutinemod
  implicit none
  type(dt(1)) :: t1a, t1b
  type(dt(2)) :: t2a, t2b

  t1a = dt(1)(127,disp1,disp2)
  t1b = dt(1)(-123,disp1a,disp2a)
  t2a = dt(2)(32000,disp1,disp2)
  t2b = dt(2)(-12345,disp1a,disp2a)

  call t1a % p1(t1a) ! 127 in disp1
  call t1a % p1(t1b) ! -123 in disp1
  call t1a % p2(t2a) ! 32000 in disp2
  call t1a % p2(t2b) ! -12345 in disp2

  call t1b % p1(t1a) ! 127 in disp1a
  call t1b % p1(t1b) ! -123 in disp1a
  call t1b % p2(t2a) ! 32000 in disp2a
  call t1b % p2(t2b) ! -12345 in disp2a

  call t2a % p1(t1a) ! 127 in disp1
  call t2a % p1(t1b) ! -123 in disp1
  call t2a % p2(t2a) ! 32000 in disp2
  call t2a % p2(t2b) ! -12345 in disp2

  call t2b % p1(t1a) ! 127 in disp1a
  call t2b % p1(t1b) ! -123 in disp1a
  call t2b % p2(t2a) ! 32000 in disp2a
  call t2b % p2(t2b) ! -12345 in disp2a

  t1a % p1 => disp1a
  t1a % p2 => disp2a

  t1b % p1 => disp1
  t1b % p2 => disp2

  t2a % p1 => disp1a
  t2a % p2 => disp2a

  t2b % p1 => disp1
  t2b % p2 => disp2

  call t1a % p1(t1a) ! 127 in disp1a
  call t1a % p1(t1b) ! -123 in disp1a
  call t1a % p2(t2a) ! 32000 in disp2a
  call t1a % p2(t2b) ! -12345 in disp2a

  call t1b % p1(t1a) ! 127 in disp1
  call t1b % p1(t1b) ! -123 in disp1
  call t1b % p2(t2a) ! 32000 in disp2
  call t1b % p2(t2b) ! -12345 in disp2

  call t2a % p1(t1a) ! 127 in disp1a
  call t2a % p1(t1b) ! -123 in disp1a
  call t2a % p2(t2a) ! 32000 in disp2a
  call t2a % p2(t2b) ! -12345 in disp2a

  call t2b % p1(t1a) ! 127 in disp1
  call t2b % p1(t1b) ! -123 in disp1
  call t2b % p2(t2a) ! 32000 in disp2
  call t2b % p2(t2b) ! -12345 in disp2

  print *, "done"

end program dtpPPCBasicNoPassSubroutine
