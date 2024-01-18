!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCDummyNPassDTPImplIntrinKLFunIntr
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to nopass function returning intrinsic type (implicit interface), DTP arg w/ K+L parameters, via dummy arg ref
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalNPassDTPInterfaceKLFunIntr (<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to functions which
!*  do not expect a passed-object dummy argument, but do expect DTP
!*  args and return an intrinsic type.  Define a parameterised derived
!*  type with procedure pointers and create instances of those types,
!*  initialising them with a structure constructor containing a
!*  reference to one of a pair of routines.  Invoke the referenced
!*  procedure via the pointer several times, and then assign new
!*  procedure references and repeat.  Uses dummy args to reference
!*  objects.  The interface is implicit, from the intrinsic type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyNPassDTPImplIntrinKLFunIntrmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (fun1), pointer, nopass :: p1 => null()
     procedure (fun2), pointer, nopass :: p2 => null()
     procedure (fun4), pointer, nopass :: p4 => null()
  end type dt


contains


  character(40) function fun1(this)
    class(dt(1,*)), intent(in) :: this
    write(fun1,*) "in fun1:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun1

  character(40) function fun2(this)
    class(dt(2,*)), intent(in) :: this
    write(fun2,*) "in fun2:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun2

  character(40) function fun4(this)
    class(dt(4,*)), intent(in) :: this
    write(fun4,*) "in fun4:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval(2:this%l-1), "<"
  end function fun4


  character(40) function fun1a(this)
    class(dt(1,*)), intent(in) :: this
    write(fun1a,*) "in fun1a:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun1a

  character(40) function fun2a(this)
    class(dt(2,*)), intent(in) :: this
    write(fun2a,*) "in fun2a:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun2a

end module dtpPPCDummyNPassDTPImplIntrinKLFunIntrmod


program dtpPPCDummyNPassDTPImplIntrinKLFunIntr

  use dtpPPCDummyNPassDTPImplIntrinKLFunIntrmod
  implicit none
  type(dt(1,0)) :: t1a_h, t1b_h
  type(dt(2,4)) :: t2a_h, t2b_h
  type(dt(1,3)) :: t1c_h, t1d_h
  type(dt(1,:)) :: t1p_h
  type(dt(4,:)) :: t4p_h
  target  :: t1c_h
  pointer :: t1p_h, t4p_h

  call test
  print *, "done"

contains

  subroutine test
    t1a_h = dt(1,0)("",127,fun1,fun2,fun4)
    t1b_h = dt(1,0)("",-127,fun1a,fun2a,fun4)
    t2a_h = dt(2,4)("abcd",32000,fun1,fun2,fun4)
    t2b_h = dt(2,4)("efgh",-12345,fun1a,fun2a,fun4)
    t1c_h = dt(1,3)("ijk",1,fun1,fun2,fun4)
    t1d_h = dt(1,3)("lmn",-1,fun1a,fun2a,fun4)
    t1p_h => t1c_h

    call runtest1(t1a_h, t1b_h, t2a_h, t2b_h, t1c_h, t1d_h, t1p_h, t4p_h)

    t1a_h % p1 => fun1a
    t1a_h % p2 => fun2a

    t1b_h % p1 => fun1
    t1b_h % p2 => fun2

    t1c_h % p1 => fun1a
    t1c_h % p2 => fun2a

    t1d_h % p1 => fun1
    t1d_h % p2 => fun2

    t2a_h % p1 => fun1a
    t2a_h % p2 => fun2a

    t2b_h % p1 => fun1
    t2b_h % p2 => fun2

    call runtest2(t1a_h, t1b_h, t2a_h, t2b_h, t1c_h, t1d_h, t1p_h, t4p_h)
    call runtest3(t1p_h, t2a_h, t4p_h)

  end subroutine test


  subroutine runtest1(t1a, t1b, t2a, t2b, t1c, t1d, t1p, t4p)
    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(1,3)) :: t1c, t1d
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    pointer :: t1p, t4p
    character(40) :: output

    print *, t1a % p1(t1a)
    print *, t1a % p1(t1b)
    print *, t1a % p1(t1c)
    print *, t1a % p1(t1d)
    print *, t1a % p1(t1p)
    print *, t1a % p2(t2a)
    print *, t1a % p2(t2b)
    print *

    print *, t1b % p1(t1a)
    print *, t1b % p1(t1b)
    print *, t1b % p1(t1c)
    print *, t1b % p1(t1d)
    print *, t1b % p1(t1p)
    print *, t1b % p2(t2a)
    print *, t1b % p2(t2b)
    print *

    print *, t2a % p1(t1a)
    print *, t2a % p1(t1b)
    print *, t2a % p1(t1c)
    print *, t2a % p1(t1d)
    print *, t2a % p1(t1p)
    print *, t2a % p2(t2a)
    print *, t2a % p2(t2b)
    print *

    print *, t2b % p1(t1a)
    print *, t2b % p1(t1b)
    print *, t2b % p1(t1c)
    print *, t2b % p1(t1d)
    print *, t2b % p1(t1p)
    print *, t2b % p2(t2a)
    print *, t2b % p2(t2b)
    print *

    print *, t1c % p1(t1a)
    print *, t1c % p1(t1b)
    print *, t1c % p1(t1c)
    print *, t1c % p1(t1d)
    print *, t1c % p1(t1p)
    print *, t1c % p2(t2a)
    print *, t1c % p2(t2b)
    print *

    print *, t1d % p1(t1a)
    print *, t1d % p1(t1b)
    print *, t1d % p1(t1c)
    print *, t1d % p1(t1d)
    print *, t1d % p1(t1p)
    print *, t1d % p2(t2a)
    print *, t1d % p2(t2b)
    print *

    print *, t1p % p1(t1a)
    print *, t1p % p1(t1b)
    print *, t1p % p1(t1c)
    print *, t1p % p1(t1d)
    print *, t1p % p1(t1p)
    print *, t1p % p2(t2a)
    print *, t1p % p2(t2b)
    print *

  end subroutine runtest1


  subroutine runtest2(t1a, t1b, t2a, t2b, t1c, t1d, t1p, t4p)
    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(1,3)) :: t1c, t1d
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    pointer :: t1p, t4p
    character(40) :: output

    allocate(t1p, source=dt(1,5)("opqrs",0,fun1a,fun2a,fun4))

    output = t1a % p1(t1a); print *, output
    output = t1a % p1(t1b); print *, output
    output = t1a % p1(t1c); print *, output
    output = t1a % p1(t1d); print *, output
    output = t1a % p1(t1p); print *, output
    output = t1a % p2(t2a); print *, output
    output = t1a % p2(t2b); print *, output
    print *

    output = t1b % p1(t1a); print *, output
    output = t1b % p1(t1b); print *, output
    output = t1b % p1(t1c); print *, output
    output = t1b % p1(t1d); print *, output
    output = t1b % p1(t1p); print *, output
    output = t1b % p2(t2a); print *, output
    output = t1b % p2(t2b); print *, output
    print *

    output = t2a % p1(t1a); print *, output
    output = t2a % p1(t1b); print *, output
    output = t2a % p1(t1c); print *, output
    output = t2a % p1(t1d); print *, output
    output = t2a % p1(t1p); print *, output
    output = t2a % p2(t2a); print *, output
    output = t2a % p2(t2b); print *, output
    print *

    output = t2b % p1(t1a); print *, output
    output = t2b % p1(t1b); print *, output
    output = t2b % p1(t1c); print *, output
    output = t2b % p1(t1d); print *, output
    output = t2b % p1(t1p); print *, output
    output = t2b % p2(t2a); print *, output
    output = t2b % p2(t2b); print *, output
    print *

    output = t1c % p1(t1a); print *, output
    output = t1c % p1(t1b); print *, output
    output = t1c % p1(t1c); print *, output
    output = t1c % p1(t1d); print *, output
    output = t1c % p1(t1p); print *, output
    output = t1c % p2(t2a); print *, output
    output = t1c % p2(t2b); print *, output
    print *

    output = t1d % p1(t1a); print *, output
    output = t1d % p1(t1b); print *, output
    output = t1d % p1(t1c); print *, output
    output = t1d % p1(t1d); print *, output
    output = t1d % p1(t1p); print *, output
    output = t1d % p2(t2a); print *, output
    output = t1d % p2(t2b); print *, output
    print *

    output = t1p % p1(t1a); print *, output
    output = t1p % p1(t1b); print *, output
    output = t1p % p1(t1c); print *, output
    output = t1p % p1(t1d); print *, output
    output = t1p % p1(t1p); print *, output
    output = t1p % p2(t2a); print *, output
    output = t1p % p2(t2b); print *, output
    print *

    allocate(t4p, source=dt(4,5)("opqrs",123454321,fun1,fun2,fun4))
    output = t4p % p4(t4p); print *, output
    output = t4p % p4(dt(4,3)("tuv", 543212345, fun1a, fun2a, fun4)); print *, output
    print *

  end subroutine runtest2

  subroutine runtest3(t1p, t2a, t4p)
    type(dt(2,4)) :: t2a
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    pointer :: t1p, t4p

    t1p%p1 => fun1
    t2a%p2 => fun2
    t4p%p4 => fun4

    print *, t1p % p1(t1p)
    print *, t2a % p2(t2a)
    print *, t4p % p4(t4p)
    print *
  end subroutine runtest3

end program dtpPPCDummyNPassDTPImplIntrinKLFunIntr
