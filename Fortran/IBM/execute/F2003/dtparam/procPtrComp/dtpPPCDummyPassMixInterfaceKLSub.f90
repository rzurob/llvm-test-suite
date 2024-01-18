!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : pointer to PASS subroutines from DTP with K+L param with other args of mixed types via dummy arg
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalPassMixInterfaceKLSub (<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to subroutines which
!*  expect a passed-object dummy argument and other args of mixed types.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  References objects via dummy arguments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyPassMixInterfaceKLSubmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (s1), pointer, pass     :: p1 => null()
     procedure (s1), pointer, pass(a2) :: p2 => null()
     procedure (s1), pointer, pass(a4) :: p4 => null()
     procedure (s5), pointer, pass     :: pR => null()
     procedure (s6), pointer, pass     :: pCI => null()
  end type dt

  abstract interface

     subroutine s1(a1,a2,a4)
       import :: dt
       class(dt(1,*)), intent(in) :: a1
       class(dt(2,*)), intent(in) :: a2
       class(dt(4,*)), intent(in) :: a4
     end subroutine s1

     subroutine s5(this,r1)
       import :: dt
       class(dt(2,*)), intent(in) :: this
       real(4), intent(in) :: r1
     end subroutine s5

     subroutine s6(this,ch,i1)
       import :: dt
       class(dt(4,*)), intent(in) :: this
       character(*), intent(in)   :: ch
       integer(4), intent(in)     :: i1
     end subroutine s6

  end interface


contains


  subroutine disp1(this,that,that2)
    class(dt(1,*)), intent(in) :: this
    class(dt(2,*)), intent(in) :: that
    class(dt(4,*)), intent(in) :: that2
    print *, "in disp1:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<", &
             that%ival, kind(that%ival), that%k, len(that%chval), that%l, ">", that%chval, "<", &
             that2%ival, kind(that2%ival), that2%k, len(that2%chval), that2%l, ">", that2%chval, "<"
  end subroutine disp1

  subroutine disp1a(this,that,that2)
    class(dt(1,*)), intent(in) :: this
    class(dt(2,*)), intent(in) :: that
    class(dt(4,*)), intent(in) :: that2
    print *, "in disp1a:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<", &
             that%ival, kind(that%ival), that%k, len(that%chval), that%l, ">", that%chval, "<", &
             that2%ival, kind(that2%ival), that2%k, len(that2%chval), that2%l, ">", that2%chval, "<"
  end subroutine disp1a

  subroutine dispR(this,r1)
    class(dt(2,*)), intent(in) :: this
    real(4), intent(in) :: r1
    print *, "in dispR:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<", r1
  end subroutine dispR


  subroutine dispRa(this,r1)
    class(dt(2,*)), intent(in) :: this
    real(4), intent(in) :: r1
    print *, "in dispRa:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<", r1
  end subroutine dispRa


  subroutine dispCI(this,ch,i1)
    class(dt(4,*)), intent(in) :: this
    character(*), intent(in)   :: ch
    integer(4), intent(in)     :: i1
    print *, "in dispCI:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "/", ch, "<", i1
  end subroutine dispCI


  subroutine dispCIa(this,ch,i1)
    class(dt(4,*)), intent(in) :: this
    character(*), intent(in)   :: ch
    integer(4), intent(in)     :: i1
    print *, "in dispCIa:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "/", ch, "<", i1
  end subroutine dispCIa

end module dtpPPCDummyPassMixInterfaceKLSubmod


program dtpPPCDummyPassMixInterfaceKLSub

  use dtpPPCDummyPassMixInterfaceKLSubmod
  implicit none
  type(dt(1,0)) :: t1a_h, t1b_h
  type(dt(2,4)) :: t2a_h, t2b_h
  type(dt(1,3)) :: t1c_h, t1d_h
  type(dt(1,:)) :: t1p_h
  type(dt(4,:)) :: t4ap_h, t4bp_h
  target  :: t1c_h, t1d_h
  pointer :: t1p_h, t4ap_h, t4bp_h

  t1a_h = dt(1,0)("",127,disp1,disp1,disp1,dispR,dispCI)
  t1b_h = dt(1,0)("",-127,disp1a,disp1a,disp1a,dispRa,dispCI)
  t2a_h = dt(2,4)("abcd",32000,disp1,disp1,disp1,dispR,dispCI)
  t2b_h = dt(2,4)("efgh",-12345,disp1a,disp1a,disp1a,dispRa,dispCI)
  t1c_h = dt(1,3)("ijk",1,disp1,disp1,disp1,dispR,dispCI)
  t1d_h = dt(1,3)("lmn",-1,disp1a,disp1a,disp1a,dispRa,dispCI)
  t1p_h => t1c_h
  allocate(t4ap_h, source=dt(4,5)("opqrs",123454321,disp1,disp1,disp1,dispR,dispCI))
  allocate(t4bp_h, source=dt(4,5)("tuvwx",123454321,disp1a,disp1a,disp1a,dispRa,dispCIa))

  call test

contains

  subroutine test
    call runtest(t1a_h, t1b_h, t2a_h, t2b_h, t1c_h, t1d_h, t1p_h, t4ap_h, t4bp_h)
  end subroutine test

  subroutine runtest(t1a, t1b, t2a, t2b, t1c, t1d, t1p, t4ap, t4bp)
    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(1,3)) :: t1c, t1d
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4ap, t4bp
    target  :: t1c, t1d
    pointer :: t1p, t4ap, t4bp

    call t1a % p1(t2a, t4ap)
    call t2a % p2(t1a, t4ap)
    call t4ap % p4(t1a, t2a)
    call t2a % pR(5.1)
    call t4ap % pCI("tuvw", 123454321)
    print *

    call t1b % p1(t2a, t4ap)
    call t2b % p2(t1a, t4ap)
    call t4bp % p4(t1a, t2a)
    call t2b % pR(5.1)
    call t4bp % pCI("xyzab", 123454321)
    print *

    call t1p % p1(t2a, t4ap)
    call t2a % p2(t1p, t4ap)
    call t4ap % p4(t1p, t2a)
    print *

    call t1d % p1(t2b, t4bp)
    call t2a % p2(t1d, t4bp)
    call t4ap % p4(t1d, t2b)
    print *

    call t2b % p2(t1d, t4bp)
    call t4bp % p4(t1d, t2b)
    print *

    ! Change some of the pointers (but not all - we want to be sure the right ones are invoked)
    t1a % p1 => disp1a
    t1a % p4 => disp1a
    t1a % pR => dispRa
    t1a % pCI=> dispCIa

    t1b % p4 => disp1

    t1c % p1 => disp1a
    t1c % p1 => disp1a
    t1c % p1 => disp1a
    t1c % pR => dispRa

    t1d % p1 => disp1
    t1d % pCI=> dispCI

    t2a % p2 => disp1a
    t2a % pR => dispRa

    t2b % p4 => disp1
    t2b % pR => dispR

    t4ap%p1 => t4bp%p1
    t4ap%p2 => t4bp%p2

    t4bp%p2 => disp1
    t4bp%p4 => t4ap%p4
    t4bp%pCI=> t4ap%pCI

    t1p => t1d

    call t1a % p1(t2a, t4ap)
    call t2a % p2(t1a, t4ap)
    call t4ap % p4(t1a, t2a)
    call t2a % pR(5.1)
    call t4ap % pCI("cd", 123454321)
    print *

    call t1b % p1(t2a, t4ap)
    call t2b % p2(t1a, t4ap)
    call t4bp % p4(t1a, t2a)
    call t2b % pR(5.1)
    call t4bp % pCI("efghijkl", 123454321)
    print *

    call t1c % p1(t2a, t4ap)
    call t2a % p2(t1c, t4ap)
    call t4ap % p4(t1c, t2a)
    print *

    call t1d % p1(t2b, t4bp)
    call t2a % p2(t1d, t4bp)
    call t4ap % p4(t1d, t2b)
    print *

    call t2b % p2(t1d, t4bp)
    call t4bp % p4(t1d, t2b)
    print *


    call t4ap % p4(dt(1,4)("WXYZ",11111111,disp1,disp1a,disp1,dispR,dispCIa), &
                   dt(2,2)("AB",  22222222,disp1a,disp1,disp1a,dispRa,dispCI))

    print *, "done"

  end subroutine runtest

end program dtpPPCDummyPassMixInterfaceKLSub
