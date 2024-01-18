!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to function without passed-object dummy argument, DTP arg w/ K+L parameters, DTP result
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
!*  Create procedure pointers which are references to functions which return a
!*  value of derived type and do not expect a passed-object dummy argument.
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

module dtpPPCLocalNPassDTPInterfaceKLFunDTPmod

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

     type(dt(1,40)) function f1(a1)
       import :: dt
       class(dt(1,*)), intent(in) :: a1
     end function f1

     type(dt(2,40)) function f2(a1)
       import :: dt
       class(dt(2,*)), intent(in) :: a1
     end function f2

     type(dt(4,40)) function f4(a1)
       import :: dt
       class(dt(4,*)), intent(in) :: a1
     end function f4

  end interface


contains


  type(dt(1,40)) function fun1(this)
    class(dt(1,*)), intent(in) :: this
    fun1 % ival = this % ival
    write(fun1 % chval,*) "in fun1:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun1

  type(dt(2,40)) function fun2(this)
    class(dt(2,*)), intent(in) :: this
    fun2 % ival = this % ival
    write(fun2 % chval,*) "in fun2:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun2


  type(dt(4,40)) function fun4(this)
    class(dt(4,*)), intent(in) :: this
    fun4 % ival = this % ival
    write(fun4 % chval,*) "in fun4:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval(2:this%l-1), "<"
  end function fun4


  type(dt(1,40)) function fun1a(this)
    class(dt(1,*)), intent(in) :: this
    fun1a % ival = this % ival
    write(fun1a % chval,*) "in fun1a:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun1a

  type(dt(2,40)) function fun2a(this)
    class(dt(2,*)), intent(in) :: this
    fun2a % ival = this % ival
    write(fun2a % chval,*) "in fun2a:", this%ival, kind(this%ival), this%k, len(this%chval), this%l, ">", this%chval, "<"
  end function fun2a


  subroutine display(this, exp)
    class(*), intent(in) :: this
    integer, intent(in) :: exp
    integer :: found
    select type (this)
    type is (dt(1,*)); print *, this%ival, trim(this%chval); found = 1
    type is (dt(2,*)); print *, this%ival, trim(this%chval); found = 2
    type is (dt(4,*)); print *, this%ival, trim(this%chval); found = 4
    class default;     print *, "Unkown type"
                       stop 2
    end select
    if (exp /= found) then
       print *, "Wrong kind. Expected", exp, "found", found
       stop 3
    end if
  end subroutine display

end module dtpPPCLocalNPassDTPInterfaceKLFunDTPmod


program dtpPPCLocalNPassDTPInterfaceKLFunDTP

  use dtpPPCLocalNPassDTPInterfaceKLFunDTPmod
  implicit none
  type(dt(1,0)) :: t1a, t1b
  type(dt(1,40)):: t1res
  type(dt(2,4)) :: t2a, t2b
  type(dt(2,40)):: t2res
  type(dt(1,3)) :: t1c, t1d
  type(dt(1,:)) :: t1p
  type(dt(4,:)) :: t4p
  type(dt(4,40)):: t4res
  target  :: t1c
  pointer :: t1p, t4p

  t1a = dt(1,0)("",127,fun1,fun2,fun4)
  t1b = dt(1,0)("",-127,fun1a,fun2a,fun4)
  t2a = dt(2,4)("abcd",32000,fun1,fun2,fun4)
  t2b = dt(2,4)("efgh",-12345,fun1a,fun2a,fun4)
  t1c = dt(1,3)("ijk",1,fun1,fun2,fun4)
  t1d = dt(1,3)("lmn",-1,fun1a,fun2a,fun4)
  t1p => t1c

  call display(t1a % p1(t1a), 1) ! 127 in fun1: 127 1 1 0 0 ><
  call display(t1a % p1(t1b), 1) ! -127 in fun1: -127 1 1 0 0 ><
  call display(t1a % p1(t1c), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t1a % p1(t1d), 1) ! -1 in fun1: -1 1 1 3 3 >lmn<
  call display(t1a % p1(t1p), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t1a % p2(t2a), 2) ! 32000 in fun2: 32000 2 2 4 4 >abcd<
  call display(t1a % p2(t2b), 2) ! -12345 in fun2: -12345 2 2 4 4 >efgh<
  print *

  call display(t1b % p1(t1a), 1) ! 127 in fun1a: 127 1 1 0 0 ><
  call display(t1b % p1(t1b), 1) ! -127 in fun1a: -127 1 1 0 0 ><
  call display(t1b % p1(t1c), 1) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  call display(t1b % p1(t1d), 1) ! -1 in fun1a: -1 1 1 3 3 >lmn<
  call display(t1b % p1(t1p), 1) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  call display(t1b % p2(t2a), 2) ! 32000 in fun2a: 32000 2 2 4 4 >abcd<
  call display(t1b % p2(t2b), 2) ! -12345 in fun2a: -12345 2 2 4 4 >efgh<
  print *

  call display(t2a % p1(t1a), 1) ! 127 in fun1: 127 1 1 0 0 ><
  call display(t2a % p1(t1b), 1) ! -127 in fun1: -127 1 1 0 0 ><
  call display(t2a % p1(t1c), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t2a % p1(t1d), 1) ! -1 in fun1: -1 1 1 3 3 >lmn<
  call display(t2a % p1(t1p), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t2a % p2(t2a), 2) ! 32000 in fun2: 32000 2 2 4 4 >abcd<
  call display(t2a % p2(t2b), 2) ! -12345 in fun2: -12345 2 2 4 4 >efgh<
  print *

  call display(t2b % p1(t1a), 1) ! 127 in fun1a: 127 1 1 0 0 ><
  call display(t2b % p1(t1b), 1) ! -127 in fun1a: -127 1 1 0 0 ><
  call display(t2b % p1(t1c), 1) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  call display(t2b % p1(t1d), 1) ! -1 in fun1a: -1 1 1 3 3 >lmn<
  call display(t2b % p1(t1p), 1) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  call display(t2b % p2(t2a), 2) ! 32000 in fun2a: 32000 2 2 4 4 >abcd<
  call display(t2b % p2(t2b), 2) ! -12345 in fun2a: -12345 2 2 4 4 >efgh<
  print *

  call display(t1c % p1(t1a), 1) ! 127 in fun1: 127 1 1 0 0 ><
  call display(t1c % p1(t1b), 1) ! -127 in fun1: -127 1 1 0 0 ><
  call display(t1c % p1(t1c), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t1c % p1(t1d), 1) ! -1 in fun1: -1 1 1 3 3 >lmn<
  call display(t1c % p1(t1p), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t1c % p2(t2a), 2) ! 32000 in fun2: 32000 2 2 4 4 >abcd<
  call display(t1c % p2(t2b), 2) ! -12345 in fun2: -12345 2 2 4 4 >efgh<
  print *

  call display(t1d % p1(t1a), 1) ! 127 in fun1a: 127 1 1 0 0 ><
  call display(t1d % p1(t1b), 1) ! -127 in fun1a: -127 1 1 0 0 ><
  call display(t1d % p1(t1c), 1) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  call display(t1d % p1(t1d), 1) ! -1 in fun1a: -1 1 1 3 3 >lmn<
  call display(t1d % p1(t1p), 1) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  call display(t1d % p2(t2a), 2) ! 32000 in fun2a: 32000 2 2 4 4 >abcd<
  call display(t1d % p2(t2b), 2) ! -12345 in fun2a: -12345 2 2 4 4 >efgh<
  print *

  call display(t1p % p1(t1a), 1) ! 127 in fun1: 127 1 1 0 0 ><
  call display(t1p % p1(t1b), 1) ! -127 in fun1: -127 1 1 0 0 ><
  call display(t1p % p1(t1c), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t1p % p1(t1d), 1) ! -1 in fun1: -1 1 1 3 3 >lmn<
  call display(t1p % p1(t1p), 1) ! 1 in fun1: 1 1 1 3 3 >ijk<
  call display(t1p % p2(t2a), 2) ! 32000 in fun2: 32000 2 2 4 4 >abcd<
  call display(t1p % p2(t2b), 2) ! -12345 in fun2: -12345 2 2 4 4 >efgh<
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

  t1res = t1a % p1(t1a); print *, t1res % ival, trim(t1res%chval) ! 127 in fun1a: 127 1 1 0 0 ><
  t1res = t1a % p1(t1b); print *, t1res % ival, trim(t1res%chval) ! -127 in fun1a: -127 1 1 0 0 ><
  t1res = t1a % p1(t1c); print *, t1res % ival, trim(t1res%chval) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  t1res = t1a % p1(t1d); print *, t1res % ival, trim(t1res%chval) ! -1 in fun1a: -1 1 1 3 3 >lmn<
  t1res = t1a % p1(t1p); print *, t1res % ival, trim(t1res%chval) ! 0 in fun1a: 0 1 1 5 5 >opqrs<
  t2res = t1a % p2(t2a); print *, t2res % ival, trim(t2res%chval) ! 32000 in fun2a: 32000 2 2 4 4 >abcd<
  t2res = t1a % p2(t2b); print *, t2res % ival, trim(t2res%chval) ! -12345 in fun2a: -12345 2 2 4 4 >efgh<
  print *

  t1res = t1b % p1(t1a); print *, t1res % ival, trim(t1res%chval) ! 127 in fun1: 127 1 1 0 0 ><
  t1res = t1b % p1(t1b); print *, t1res % ival, trim(t1res%chval) ! -127 in fun1: -127 1 1 0 0 ><
  t1res = t1b % p1(t1c); print *, t1res % ival, trim(t1res%chval) ! 1 in fun1: 1 1 1 3 3 >ijk<
  t1res = t1b % p1(t1d); print *, t1res % ival, trim(t1res%chval) ! -1 in fun1: -1 1 1 3 3 >lmn<
  t1res = t1b % p1(t1p); print *, t1res % ival, trim(t1res%chval) ! 0 in fun1: 0 1 1 5 5 >opqrs<
  t2res = t1b % p2(t2a); print *, t2res % ival, trim(t2res%chval) ! 32000 in fun2: 32000 2 2 4 4 >abcd<
  t2res = t1b % p2(t2b); print *, t2res % ival, trim(t2res%chval) ! -12345 in fun2: -12345 2 2 4 4 >efgh<
  print *

  t1res = t2a % p1(t1a); print *, t1res % ival, trim(t1res%chval) ! 127 in fun1a: 127 1 1 0 0 ><
  t1res = t2a % p1(t1b); print *, t1res % ival, trim(t1res%chval) ! -127 in fun1a: -127 1 1 0 0 ><
  t1res = t2a % p1(t1c); print *, t1res % ival, trim(t1res%chval) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  t1res = t2a % p1(t1d); print *, t1res % ival, trim(t1res%chval) ! -1 in fun1a: -1 1 1 3 3 >lmn<
  t1res = t2a % p1(t1p); print *, t1res % ival, trim(t1res%chval) ! 0 in fun1a: 0 1 1 5 5 >opqrs<
  t2res = t2a % p2(t2a); print *, t2res % ival, trim(t2res%chval) ! 32000 in fun2a: 32000 2 2 4 4 >abcd<
  t2res = t2a % p2(t2b); print *, t2res % ival, trim(t2res%chval) ! -12345 in fun2a: -12345 2 2 4 4 >efgh<
  print *

  t1res = t2b % p1(t1a); print *, t1res % ival, trim(t1res%chval) ! 127 in fun1: 127 1 1 0 0 ><
  t1res = t2b % p1(t1b); print *, t1res % ival, trim(t1res%chval) ! -127 in fun1: -127 1 1 0 0 ><
  t1res = t2b % p1(t1c); print *, t1res % ival, trim(t1res%chval) ! 1 in fun1: 1 1 1 3 3 >ijk<
  t1res = t2b % p1(t1d); print *, t1res % ival, trim(t1res%chval) ! -1 in fun1: -1 1 1 3 3 >lmn<
  t1res = t2b % p1(t1p); print *, t1res % ival, trim(t1res%chval) ! 0 in fun1: 0 1 1 5 5 >opqrs<
  t2res = t2b % p2(t2a); print *, t2res % ival, trim(t2res%chval) ! 32000 in fun2: 32000 2 2 4 4 >abcd<
  t2res = t2b % p2(t2b); print *, t2res % ival, trim(t2res%chval) ! -12345 in fun2: -12345 2 2 4 4 >efgh<
  print *

  t1res = t1c % p1(t1a); print *, t1res % ival, trim(t1res%chval) ! 127 in fun1a: 127 1 1 0 0 ><
  t1res = t1c % p1(t1b); print *, t1res % ival, trim(t1res%chval) ! -127 in fun1a: -127 1 1 0 0 ><
  t1res = t1c % p1(t1c); print *, t1res % ival, trim(t1res%chval) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  t1res = t1c % p1(t1d); print *, t1res % ival, trim(t1res%chval) ! -1 in fun1a: -1 1 1 3 3 >lmn<
  t1res = t1c % p1(t1p); print *, t1res % ival, trim(t1res%chval) ! 0 in fun1a: 0 1 1 5 5 >opqrs<
  t2res = t1c % p2(t2a); print *, t2res % ival, trim(t2res%chval) ! 32000 in fun2a: 32000 2 2 4 4 >abcd<
  t2res = t1c % p2(t2b); print *, t2res % ival, trim(t2res%chval) ! -12345 in fun2a: -12345 2 2 4 4 >efgh<
  print *

  t1res = t1d % p1(t1a); print *, t1res % ival, trim(t1res%chval) ! 127 in fun1: 127 1 1 0 0 ><
  t1res = t1d % p1(t1b); print *, t1res % ival, trim(t1res%chval) ! -127 in fun1: -127 1 1 0 0 ><
  t1res = t1d % p1(t1c); print *, t1res % ival, trim(t1res%chval) ! 1 in fun1: 1 1 1 3 3 >ijk<
  t1res = t1d % p1(t1d); print *, t1res % ival, trim(t1res%chval) ! -1 in fun1: -1 1 1 3 3 >lmn<
  t1res = t1d % p1(t1p); print *, t1res % ival, trim(t1res%chval) ! 0 in fun1: 0 1 1 5 5 >opqrs<
  t2res = t1d % p2(t2a); print *, t2res % ival, trim(t2res%chval) ! 32000 in fun2: 32000 2 2 4 4 >abcd<
  t2res = t1d % p2(t2b); print *, t2res % ival, trim(t2res%chval) ! -12345 in fun2: -12345 2 2 4 4 >efgh<
  print *

  t1res = t1p % p1(t1a); print *, t1res % ival, trim(t1res%chval) ! 127 in fun1a: 127 1 1 0 0 ><
  t1res = t1p % p1(t1b); print *, t1res % ival, trim(t1res%chval) ! -127 in fun1a: -127 1 1 0 0 ><
  t1res = t1p % p1(t1c); print *, t1res % ival, trim(t1res%chval) ! 1 in fun1a: 1 1 1 3 3 >ijk<
  t1res = t1p % p1(t1d); print *, t1res % ival, trim(t1res%chval) ! -1 in fun1a: -1 1 1 3 3 >lmn<
  t1res = t1p % p1(t1p); print *, t1res % ival, trim(t1res%chval) ! 0 in fun1a: 0 1 1 5 5 >opqrs<
  t2res = t1p % p2(t2a); print *, t2res % ival, trim(t2res%chval) ! 32000 in fun2a: 32000 2 2 4 4 >abcd<
  t2res = t1p % p2(t2b); print *, t2res % ival, trim(t2res%chval) ! -12345 in fun2a: -12345 2 2 4 4 >efgh<
  print *

  allocate(t4p, source=dt(4,5)("opqrs",123454321,fun1,fun2,fun4))
  call display(t4p % p4(t4p), 4) ! 123454321 in fun4: 123454321 4 4 5 5 >pqr<
  t4res = t4p % p4(t4p); print *, t4res % ival, trim(t4res%chval) ! 123454321 in fun4: 123454321 4 4 5 5 >pqr<

  call display(t4p % p4(dt(4,3)("tuv", 543212345, fun1a, fun2a, fun4)), 4) ! 543212345 in fun4: 543212345 4 4 3 3 >u<
  t4res = t4p % p4(dt(4,3)("tuv", 543212345, fun1a, fun2a, fun4)); print *, t4res % ival, trim(t4res%chval) ! 543212345 in fun4: 543212345 4 4 3 3 >u<
  print *

  print *, "done"

end program dtpPPCLocalNPassDTPInterfaceKLFunDTP
