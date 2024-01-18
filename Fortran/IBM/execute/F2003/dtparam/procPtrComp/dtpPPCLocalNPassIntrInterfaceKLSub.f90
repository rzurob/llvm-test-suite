!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to subroutine without passed-object dummy argument, DTP arg w/ K+L parameters, arg of intrinsic type
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
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.  The args which are passed in
!*  are of intrinsic type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalNPassIntrInterfaceKLSubmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (s1), pointer, nopass :: p1 => null()
     procedure (s2), pointer, nopass :: p2 => null()
     procedure (s4), pointer, nopass :: p4 => null()
  end type dt

  abstract interface

     subroutine s1(a1)
       character(*), intent(in) :: a1
     end subroutine s1

     subroutine s2(a1)
       integer(4), intent(in) :: a1
     end subroutine s2

     subroutine s4(a1)
       import :: dt
       integer(2), intent(in) :: a1
     end subroutine s4

  end interface


contains


  subroutine disp1(this)
    character(*), intent(in) :: this
    print *, "in disp1: >", this, "<"
  end subroutine disp1

  subroutine disp2(this)
    integer(4), intent(in) :: this
    print *, "in disp2:", this
  end subroutine disp2

  subroutine disp4(this)
    integer(2), intent(in) :: this
    print *, "in disp4:", this
  end subroutine disp4

  subroutine disp1a(this)
    character(*), intent(in) :: this
    print *, "in disp1a: >", this, "<"
  end subroutine disp1a

  subroutine disp2a(this)
    integer(4), intent(in) :: this
    print *, "in disp2a:", this
  end subroutine disp2a

end module dtpPPCLocalNPassIntrInterfaceKLSubmod


program dtpPPCLocalNPassIntrInterfaceKLSub

  use dtpPPCLocalNPassIntrInterfaceKLSubmod
  implicit none
  type(dt(1,0)) :: t1a, t1b
  type(dt(2,4)) :: t2a
  type(dt(1,3)) :: t1c
  type(dt(1,:)) :: t1p
  type(dt(4,:)) :: t4p
  target  :: t1c
  pointer :: t1p, t4p

  t1a = dt(1,0)("",127,disp1,disp2,disp4)
  t1b = dt(1,0)("",-127,disp1a,disp2a,disp4)
  t1c = dt(1,3)("ijk",1,disp1,disp2,disp4)
  t1p => t1c
  t2a = dt(2,4)("abcd",32000,disp1,disp2,disp4)

  call t1a % p1("abc")      ! in disp1: >abc<
  call t1a % p1(" ")        ! in disp1: > <
  call t1a % p1("")         ! in disp1: ><
  call t1a % p2(12345678)   ! in disp2: 12345678
  call t1a % p4(12345_2)    ! in disp4: 12345
  print *

  call t1b % p1("defg")     ! in disp1a: >defg<
  call t1b % p1(" ")        ! in disp1a: > <
  call t1b % p1("")         ! in disp1a: ><
  call t1b % p2(87654321)   ! in disp2a: 87654321
  call t1b % p4(-1_2)       ! in disp4: -1
  print *

  call t1c % p1("hijklm")   ! in disp1: >hijklm<
  call t1c % p1(" ")        ! in disp1: > <
  call t1c % p1("")         ! in disp1: ><
  call t1c % p2(-1111111)   ! in disp2: -1111111
  call t1c % p4(32765_2)    ! in disp4: 32765
  print *

  call t1p % p1("hijklm")   ! in disp1: >hijklm<
  call t1p % p1(" ")        ! in disp1: > <
  call t1p % p1("")         ! in disp1: ><
  call t1p % p2(-1111111)   ! in disp2: -1111111
  call t1p % p4(32765_2)    ! in disp4: 32765
  print *

  call t2a % p1("nopqrs")   ! in disp1: >nopqrs<
  call t2a % p1("tuv")      ! in disp1: >tuv<
  call t2a % p2(2139062143) ! in disp2: 2139062143
  call t2a % p2(-16843009)  ! in disp2: -16843009
  call t2a % p4(-16843_2)   ! in disp4: -16843
  print *

  t1a % p1 => disp1a
  t1a % p2 => disp2a

  t1b % p1 => disp1
  t1b % p2 => disp2

  t1c % p1 => disp1a
  t1c % p2 => disp2a

  t2a % p1 => disp1a
  t2a % p2 => disp2a

  allocate(t1p, source=dt(1,5)("opqrs",0,disp1a,disp2a,disp4))

  call t1a % p1("abc")      ! in disp1a: >abc<
  call t1a % p1(" ")        ! in disp1a: > <
  call t1a % p1("")         ! in disp1a: ><
  call t1a % p2(12345678)   ! in disp2a: 12345678
  call t1a % p4(12345_2)    ! in disp4: 12345
  print *

  call t1b % p1("defg")     ! in disp1: >defg<
  call t1b % p1(" ")        ! in disp1: > <
  call t1b % p1("")         ! in disp1: ><
  call t1b % p2(87654321)   ! in disp2: 87654321
  call t1b % p4(-1_2)       ! in disp4: -1
  print *

  call t1c % p1("hijklm")   ! in disp1a: >hijklm<
  call t1c % p1(" ")        ! in disp1a: > <
  call t1c % p1("")         ! in disp1a: ><
  call t1c % p2(-1111111)   ! in disp2a: -1111111
  call t1c % p4(32765_2)    ! in disp4: 32765
  print *

  call t1p % p1("hijklm")   ! in disp1a: >hijklm<
  call t1p % p1(" ")        ! in disp1a: > <
  call t1p % p1("")         ! in disp1a: ><
  call t1p % p2(-1111111)   ! in disp2a: -1111111
  call t1p % p4(32765_2)    ! in disp4: 32765
  print *

  call t2a % p1("nopqrs")   ! in disp1a: >nopqrs<
  call t2a % p1("tuv")      ! in disp1a: >tuv<
  call t2a % p2(2139062143) ! in disp2a: 2139062143
  call t2a % p2(-16843009)  ! in disp2a: -16843009
  call t2a % p4(-16843_2)   ! in disp4: -16843
  print *

  allocate(t4p, source=dt(4,5)("opqrs",123454321,disp1,disp2,disp4))
  call t4p % p1("t4p")      ! in disp1: >t4p<
  call t4p % p2(444444444)  ! in disp2: 444444444
  call t4p % p4(4444_2)     ! in disp4: 4444
  print *

  print *, "done"

end program dtpPPCLocalNPassIntrInterfaceKLSub
