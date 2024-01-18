!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCDummyNPassDTPInterfaceKLSub
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-03-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : pointer to NOPASS subroutines from DTP with K+L param with other args of mixed types, dummy
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpPPCLocalNPassDTPInterfaceKLSub (<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to subroutines which
!*  do not expect a passed-object dummy argument.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  References objects via dummy arguments, which are in turn host associated.
!*  The arg which is passed in has kind and len parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyNPassDTPInterfaceKLSubmod

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
       import :: dt
       class(dt(1,*)), intent(in) :: a1
     end subroutine s1

     subroutine s2(a1)
       import :: dt
       class(dt(2,*)), intent(in) :: a1
     end subroutine s2

     subroutine s4(a1)
       import :: dt
       class(dt(4,*)), intent(in) :: a1
     end subroutine s4

  end interface


contains


  subroutine disp1(this)
    class(dt(1,*)), intent(in) :: this
    print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp1"
  end subroutine disp1

  subroutine disp2(this)
    class(dt(2,*)), intent(in) :: this
    print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp2"
  end subroutine disp2


  subroutine disp4(this)
    class(dt(4,*)), intent(in) :: this
    print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval(2:this%l-1), "< in disp4"
  end subroutine disp4


  subroutine disp1a(this)
    class(dt(1,*)), intent(in) :: this
    print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp1a"
  end subroutine disp1a

  subroutine disp2a(this)
    class(dt(2,*)), intent(in) :: this
    print *, this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "< in disp2a"
  end subroutine disp2a

end module dtpPPCDummyNPassDTPInterfaceKLSubmod


program dtpPPCDummyNPassDTPInterfaceKLSub

  use dtpPPCDummyNPassDTPInterfaceKLSubmod
  implicit none
  type(dt(1,0)) :: t1a_h, t1b_h
  type(dt(2,4)) :: t2a_h, t2b_h
  type(dt(1,3)) :: t1c_h, t1d_h
  type(dt(1,:)) :: t1p_h
  type(dt(4,:)) :: t4p_h
  target  :: t1c_h
  pointer :: t1p_h, t4p_h

  t1a_h = dt(1,0)("",127,disp1,disp2,disp4)
  t1b_h = dt(1,0)("",-127,disp1a,disp2a,disp4)
  t2a_h = dt(2,4)("abcd",32000,disp1,disp2,disp4)
  t2b_h = dt(2,4)("efgh",-12345,disp1a,disp2a,disp4)
  t1c_h = dt(1,3)("ijk",1,disp1,disp2,disp4)
  t1d_h = dt(1,3)("lmn",-1,disp1a,disp2a,disp4)
  t1p_h => t1c_h

  call test

contains

  subroutine test
    call runtest(t1a_h, t1b_h, t2a_h, t2b_h, t1c_h, t1d_h, t1p_h, t4p_h)
    t1a_h % p1 => disp1a
    t1a_h % p2 => disp2a

    t1b_h % p1 => disp1
    t1b_h % p2 => disp2

    t1c_h % p1 => disp1a
    t1c_h % p2 => disp2a

    t1d_h % p1 => disp1
    t1d_h % p2 => disp2

    t2a_h % p1 => disp1a
    t2a_h % p2 => disp2a

    t2b_h % p1 => disp1
    t2b_h % p2 => disp2

    allocate(t1p_h, source=dt(1,5)("opqrs",0,disp1a,disp2a,disp4))
    call runtest(t1a_h, t1b_h, t2a_h, t2b_h, t1c_h, t1d_h, t1p_h, t4p_h)

    call runtest2(t4p_h)

    print *, "done"
  end subroutine test


  subroutine runtest(t1a, t1b, t2a, t2b, t1c, t1d, t1p, t4p)
    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(1,3)) :: t1c, t1d
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    pointer :: t1p, t4p

    call t1a % p1(t1a)
    call t1a % p1(t1b)
    call t1a % p1(t1c)
    call t1a % p1(t1d)
    call t1a % p1(t1p)
    call t1a % p2(t2a)
    call t1a % p2(t2b)

    call t1b % p1(t1a)
    call t1b % p1(t1b)
    call t1b % p1(t1c)
    call t1b % p1(t1d)
    call t1b % p1(t1p)
    call t1b % p2(t2a)
    call t1b % p2(t2b)

    call t2a % p1(t1a)
    call t2a % p1(t1b)
    call t2a % p1(t1c)
    call t2a % p1(t1d)
    call t2a % p1(t1p)
    call t2a % p2(t2a)
    call t2a % p2(t2b)

    call t2b % p1(t1a)
    call t2b % p1(t1b)
    call t2b % p1(t1c)
    call t2b % p1(t1d)
    call t2b % p1(t1p)
    call t2b % p2(t2a)
    call t2b % p2(t2b)

    call t1c % p1(t1a)
    call t1c % p1(t1b)
    call t1c % p1(t1c)
    call t1c % p1(t1d)
    call t1c % p1(t1p)
    call t1c % p2(t2a)
    call t1c % p2(t2b)

    call t1d % p1(t1a)
    call t1d % p1(t1b)
    call t1d % p1(t1c)
    call t1d % p1(t1d)
    call t1d % p1(t1p)
    call t1d % p2(t2a)
    call t1d % p2(t2b)

    call t1p % p1(t1a)
    call t1p % p1(t1b)
    call t1p % p1(t1c)
    call t1p % p1(t1d)
    call t1p % p1(t1p)
    call t1p % p2(t2a)
    call t1p % p2(t2b)
  end subroutine runtest


  subroutine runtest2(t4p)
    type(dt(4,:)), pointer :: t4p
    allocate(t4p, source=dt(4,5)("opqrs",123454321,disp1,disp2,disp4))
    call t4p % p4(t4p)
    call t4p % p4(dt(4,3)("tuv", 543212345, disp1a, disp2a, disp4))
  end subroutine runtest2

end program dtpPPCDummyNPassDTPInterfaceKLSub
