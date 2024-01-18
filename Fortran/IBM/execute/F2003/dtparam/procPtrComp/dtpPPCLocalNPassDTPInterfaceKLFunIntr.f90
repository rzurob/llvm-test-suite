!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCLocalNPassDTPInterfaceKLFunIntr
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-03-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to function without passed-object dummy argument, DTP arg w/ K+L parameters
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
!*  Create procedure pointers which are references to functions which return a
!*  value of intrinsic type and do not expect a passed-object dummy argument.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.  The arg which is passed in has
!*  kind and len parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalNPassDTPInterfaceKLFunIntrmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (f1), pointer, nopass :: p1 => null()
     procedure (f2), pointer, nopass :: p2 => null()
     procedure (f4), pointer, nopass :: p4 => null()
  end type dt

  abstract interface

     character(40) function f1(a1)
       import :: dt
       class(dt(1,*)), intent(in) :: a1
     end function f1

     character(40) function f2(a1)
       import :: dt
       class(dt(2,*)), intent(in) :: a1
     end function f2

     character(40) function f4(a1)
       import :: dt
       class(dt(4,*)), intent(in) :: a1
     end function f4

  end interface


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

end module dtpPPCLocalNPassDTPInterfaceKLFunIntrmod


program dtpPPCLocalNPassDTPInterfaceKLFunIntr

  use dtpPPCLocalNPassDTPInterfaceKLFunIntrmod
  implicit none
  type(dt(1,0)) :: t1a, t1b
  type(dt(2,4)) :: t2a, t2b
  type(dt(1,3)) :: t1c, t1d
  type(dt(1,:)) :: t1p
  type(dt(4,:)) :: t4p
  target  :: t1c
  pointer :: t1p, t4p
  character(40) :: output

  t1a = dt(1,0)("",127,fun1,fun2,fun4)
  t1b = dt(1,0)("",-127,fun1a,fun2a,fun4)
  t2a = dt(2,4)("abcd",32000,fun1,fun2,fun4)
  t2b = dt(2,4)("efgh",-12345,fun1a,fun2a,fun4)
  t1c = dt(1,3)("ijk",1,fun1,fun2,fun4)
  t1d = dt(1,3)("lmn",-1,fun1a,fun2a,fun4)
  t1p => t1c

  print *, t1a % p1(t1a) ! in fun1: 127 1 1 0 0 ><
  print *, t1a % p1(t1b) ! in fun1: -127 1 1 0 0 ><
  print *, t1a % p1(t1c) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t1a % p1(t1d) ! in fun1: -1 1 1 3 3 >lmn<
  print *, t1a % p1(t1p) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t1a % p2(t2a) ! in fun2: 32000 2 2 4 4 >abcd<
  print *, t1a % p2(t2b) ! in fun2: -12345 2 2 4 4 >efgh<
  print *

  print *, t1b % p1(t1a) ! in fun1a: 127 1 1 0 0 ><
  print *, t1b % p1(t1b) ! in fun1a: -127 1 1 0 0 ><
  print *, t1b % p1(t1c) ! in fun1a: 1 1 1 3 3 >ijk<
  print *, t1b % p1(t1d) ! in fun1a: -1 1 1 3 3 >lmn<
  print *, t1b % p1(t1p) ! in fun1a: 1 1 1 3 3 >ijk<
  print *, t1b % p2(t2a) ! in fun2a: 32000 2 2 4 4 >abcd<
  print *, t1b % p2(t2b) ! in fun2a: -12345 2 2 4 4 >efgh<
  print *

  print *, t2a % p1(t1a) ! in fun1: 127 1 1 0 0 ><
  print *, t2a % p1(t1b) ! in fun1: -127 1 1 0 0 ><
  print *, t2a % p1(t1c) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t2a % p1(t1d) ! in fun1: -1 1 1 3 3 >lmn<
  print *, t2a % p1(t1p) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t2a % p2(t2a) ! in fun2: 32000 2 2 4 4 >abcd<
  print *, t2a % p2(t2b) ! in fun2: -12345 2 2 4 4 >efgh<
  print *

  print *, t2b % p1(t1a) ! in fun1a: 127 1 1 0 0 ><
  print *, t2b % p1(t1b) ! in fun1a: -127 1 1 0 0 ><
  print *, t2b % p1(t1c) ! in fun1a: 1 1 1 3 3 >ijk<
  print *, t2b % p1(t1d) ! in fun1a: -1 1 1 3 3 >lmn<
  print *, t2b % p1(t1p) ! in fun1a: 1 1 1 3 3 >ijk<
  print *, t2b % p2(t2a) ! in fun2a: 32000 2 2 4 4 >abcd<
  print *, t2b % p2(t2b) ! in fun2a: -12345 2 2 4 4 >efgh<
  print *

  print *, t1c % p1(t1a) ! in fun1: 127 1 1 0 0 ><
  print *, t1c % p1(t1b) ! in fun1: -127 1 1 0 0 ><
  print *, t1c % p1(t1c) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t1c % p1(t1d) ! in fun1: -1 1 1 3 3 >lmn<
  print *, t1c % p1(t1p) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t1c % p2(t2a) ! in fun2: 32000 2 2 4 4 >abcd<
  print *, t1c % p2(t2b) ! in fun2: -12345 2 2 4 4 >efgh<
  print *

  print *, t1d % p1(t1a) ! in fun1a: 127 1 1 0 0 ><
  print *, t1d % p1(t1b) ! in fun1a: -127 1 1 0 0 ><
  print *, t1d % p1(t1c) ! in fun1a: 1 1 1 3 3 >ijk<
  print *, t1d % p1(t1d) ! in fun1a: -1 1 1 3 3 >lmn<
  print *, t1d % p1(t1p) ! in fun1a: 1 1 1 3 3 >ijk<
  print *, t1d % p2(t2a) ! in fun2a: 32000 2 2 4 4 >abcd<
  print *, t1d % p2(t2b) ! in fun2a: -12345 2 2 4 4 >efgh<
  print *

  print *, t1p % p1(t1a) ! in fun1: 127 1 1 0 0 ><
  print *, t1p % p1(t1b) ! in fun1: -127 1 1 0 0 ><
  print *, t1p % p1(t1c) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t1p % p1(t1d) ! in fun1: -1 1 1 3 3 >lmn<
  print *, t1p % p1(t1p) ! in fun1: 1 1 1 3 3 >ijk<
  print *, t1p % p2(t2a) ! in fun2: 32000 2 2 4 4 >abcd<
  print *, t1p % p2(t2b) ! in fun2: -12345 2 2 4 4 >efgh<
  print *

  t1a % p1 => fun1a
  t1a % p2 => fun2a

  t1b % p1 => fun1
  t1b % p2 => fun2

  t1c % p1 => fun1a
  t1c % p2 => fun2a

  t1d % p1 => fun1
  t1d % p2 => fun2

  t2a % p1 => fun1a
  t2a % p2 => fun2a

  t2b % p1 => fun1
  t2b % p2 => fun2

  allocate(t1p, source=dt(1,5)("opqrs",0,fun1a,fun2a,fun4))

  output = t1a % p1(t1a); print *, output ! in fun1a: 127 1 1 0 0 ><
  output = t1a % p1(t1b); print *, output ! in fun1a: -127 1 1 0 0 ><
  output = t1a % p1(t1c); print *, output ! in fun1a: 1 1 1 3 3 >ijk<
  output = t1a % p1(t1d); print *, output ! in fun1a: -1 1 1 3 3 >lmn<
  output = t1a % p1(t1p); print *, output ! in fun1a: 0 1 1 5 5 >opqrs<
  output = t1a % p2(t2a); print *, output ! in fun2a: 32000 2 2 4 4 >abcd<
  output = t1a % p2(t2b); print *, output ! in fun2a: -12345 2 2 4 4 >efgh<
  print *

  output = t1b % p1(t1a); print *, output ! in fun1: 127 1 1 0 0 ><
  output = t1b % p1(t1b); print *, output ! in fun1: -127 1 1 0 0 ><
  output = t1b % p1(t1c); print *, output ! in fun1: 1 1 1 3 3 >ijk<
  output = t1b % p1(t1d); print *, output ! in fun1: -1 1 1 3 3 >lmn<
  output = t1b % p1(t1p); print *, output ! in fun1: 0 1 1 5 5 >opqrs<
  output = t1b % p2(t2a); print *, output ! in fun2: 32000 2 2 4 4 >abcd<
  output = t1b % p2(t2b); print *, output ! in fun2: -12345 2 2 4 4 >efgh<
  print *

  output = t2a % p1(t1a); print *, output ! in fun1a: 127 1 1 0 0 ><
  output = t2a % p1(t1b); print *, output ! in fun1a: -127 1 1 0 0 ><
  output = t2a % p1(t1c); print *, output ! in fun1a: 1 1 1 3 3 >ijk<
  output = t2a % p1(t1d); print *, output ! in fun1a: -1 1 1 3 3 >lmn<
  output = t2a % p1(t1p); print *, output ! in fun1a: 0 1 1 5 5 >opqrs<
  output = t2a % p2(t2a); print *, output ! in fun2a: 32000 2 2 4 4 >abcd<
  output = t2a % p2(t2b); print *, output ! in fun2a: -12345 2 2 4 4 >efgh<
  print *

  output = t2b % p1(t1a); print *, output ! in fun1: 127 1 1 0 0 ><
  output = t2b % p1(t1b); print *, output ! in fun1: -127 1 1 0 0 ><
  output = t2b % p1(t1c); print *, output ! in fun1: 1 1 1 3 3 >ijk<
  output = t2b % p1(t1d); print *, output ! in fun1: -1 1 1 3 3 >lmn<
  output = t2b % p1(t1p); print *, output ! in fun1: 0 1 1 5 5 >opqrs<
  output = t2b % p2(t2a); print *, output ! in fun2: 32000 2 2 4 4 >abcd<
  output = t2b % p2(t2b); print *, output ! in fun2: -12345 2 2 4 4 >efgh<
  print *

  output = t1c % p1(t1a); print *, output ! in fun1a: 127 1 1 0 0 ><
  output = t1c % p1(t1b); print *, output ! in fun1a: -127 1 1 0 0 ><
  output = t1c % p1(t1c); print *, output ! in fun1a: 1 1 1 3 3 >ijk<
  output = t1c % p1(t1d); print *, output ! in fun1a: -1 1 1 3 3 >lmn<
  output = t1c % p1(t1p); print *, output ! in fun1a: 0 1 1 5 5 >opqrs<
  output = t1c % p2(t2a); print *, output ! in fun2a: 32000 2 2 4 4 >abcd<
  output = t1c % p2(t2b); print *, output ! in fun2a: -12345 2 2 4 4 >efgh<
  print *

  output = t1d % p1(t1a); print *, output ! in fun1: 127 1 1 0 0 ><
  output = t1d % p1(t1b); print *, output ! in fun1: -127 1 1 0 0 ><
  output = t1d % p1(t1c); print *, output ! in fun1: 1 1 1 3 3 >ijk<
  output = t1d % p1(t1d); print *, output ! in fun1: -1 1 1 3 3 >lmn<
  output = t1d % p1(t1p); print *, output ! in fun1: 0 1 1 5 5 >opqrs<
  output = t1d % p2(t2a); print *, output ! in fun2: 32000 2 2 4 4 >abcd<
  output = t1d % p2(t2b); print *, output ! in fun2: -12345 2 2 4 4 >efgh<
  print *

  output = t1p % p1(t1a); print *, output ! in fun1a: 127 1 1 0 0 ><
  output = t1p % p1(t1b); print *, output ! in fun1a: -127 1 1 0 0 ><
  output = t1p % p1(t1c); print *, output ! in fun1a: 1 1 1 3 3 >ijk<
  output = t1p % p1(t1d); print *, output ! in fun1a: -1 1 1 3 3 >lmn<
  output = t1p % p1(t1p); print *, output ! in fun1a: 0 1 1 5 5 >opqrs<
  output = t1p % p2(t2a); print *, output ! in fun2a: 32000 2 2 4 4 >abcd<
  output = t1p % p2(t2b); print *, output ! in fun2a: -12345 2 2 4 4 >efgh<
  print *

  allocate(t4p, source=dt(4,5)("opqrs",123454321,fun1,fun2,fun4))
  output = t4p % p4(t4p); print *, output ! in fun4: 123454321 4 4 5 5 >pqr<
  output = t4p % p4(dt(4,3)("tuv", 543212345, fun1a, fun2a, fun4)); print *, output ! in fun4: 543212345 4 4 3 3 >u<
  print *

  print *, "done"

end program dtpPPCLocalNPassDTPInterfaceKLFunIntr
