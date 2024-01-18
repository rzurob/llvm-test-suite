!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to pass function with DTP host args & result (K+L), inheritance, ref via dummy arg
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCDummyPassMixInterfaceKLFunDTP (<-dtpPPCLocalPassMixInterfaceKLFunDTP<-dtpPPCLocalNPassDTPInterfaceKLFunDTP<-dtpPPCLocalNPassDTPInterfaceKLFunIntr<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to pass functions which
!*  return a value of derived type and expect DTP dummy arguments.  Define a
!*  parameterised derived type with procedure pointers and create instances of
!*  those types, initialising them with a structure constructor containing a
!*  reference to one of a pair of routines.  Invoke the referenced procedure via
!*  the pointer several times, and then assign new procedure references and
!*  repeat.  Uses dummy args to reference objects.
!*  (Types dl and dk come from dtpPPCDummyNPassIntrImplDTKLFunDTP)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyPassDTPInterfaceKLFunDTPInhmod

  implicit none

  type base (k)
     integer, kind :: k
     integer(k)    :: ival
     procedure (f1), pointer, pass :: p1 => null()
     procedure (f2), pointer, pass :: p2 => null()
     procedure (f4), pointer, pass :: p4 => null()
  end type base

  type, extends(base) :: dt (l)
     integer, len  :: l
     character(l)  :: chval
!!$     procedure (f1), pointer, pass :: p1 => null()
!!$     procedure (f2), pointer, pass :: p2 => null()
!!$     procedure (f4), pointer, pass :: p4 => null()
  end type dt

  type dl (l1,l2)
     integer, len  :: l1, l2
     character(l1) :: charr(l2)
  end type dl

  type dk (k1,k2)
     integer, kind :: k1, k2
     integer(k1) :: ival1
     integer(k2) :: ival2
     real(max(k1,k2)) :: rval
  end type dk

  abstract interface

     type(dt(1,40)) function f1(d,dl1,dk1)
       import :: dt, dl, dk, base
       class(base(1)), intent(in) :: d
       class(dl(*,*)), intent(in) :: dl1
       class(dk(1,4)), intent(in) :: dk1
     end function f1

     type(dt(2,40)) function f2(d,dk1,dl1)
       import :: dt, dl, dk, base
       class(base(2)), intent(in) :: d
       class(dk(2,4)), intent(in) :: dk1
       class(dl(*,*)), intent(in) :: dl1
     end function f2

     type(dt(4,40)) function f4(d1,dl1,dl2,dk1)
       import :: dt, dl, dk, base
       class(base(4)), intent(in) :: d1
       class(dl(*,*)), intent(in) :: dl1, dl2
       class(dk(4,1)), intent(in) :: dk1
     end function f4

  end interface


contains


  type(dt(1,40)) function fun1(this,dl1,dk1)
    class(base(1)), intent(in) :: this
    class(dl(*,*)), intent(in) :: dl1
    class(dk(1,4)), intent(in) :: dk1
    integer :: i
    fun1%ival = this%ival + dk1%ival1
    select type(this)
    type is (dt(1,*))
      write(fun1%chval,*) trim(this%chval), ("/", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class is (base(1))
      write(fun1%chval,*) "base", ("/", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class default
      stop 12
    end select
  end function fun1

  type(dt(2,40)) function fun2(this,dk1,dl1)
    class(base(2)), intent(in) :: this
    class(dk(2,4)), intent(in) :: dk1
    class(dl(*,*)), intent(in) :: dl1
    integer :: i
    fun2%ival = int(this%ival * dk1%rval, 2)
    select type(this)
    type is (dt(2,*))
      write(fun2%chval,*) trim(this%chval), (":", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class is (base(2))
      write(fun2%chval,*) "base", (":", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class default
      stop 13
    end select
  end function fun2

  type(dt(4,40)) function fun4(this,dl1,dl2,dk1)
    class(base(4)), intent(in) :: this
    class(dl(*,*)), intent(in) :: dl1, dl2
    class(dk(4,1)), intent(in) :: dk1
    integer :: i
    fun4%ival = this%ival + dk1%ival1 + dk1%ival2
    select type(this)
    type is (dt(4,*))
      write(fun4%chval,*) this%chval, (";", trim(dl1%charr(i)), ",", trim(dl2%charr(i)), i=1,min(size(dl1%charr),size(dl2%charr)))
    class is (base(4))
      write(fun4%chval,*) "base", (";", trim(dl1%charr(i)), ",", trim(dl2%charr(i)), i=1,min(size(dl1%charr),size(dl2%charr)))
    class default
      stop 14
    end select
  end function fun4


  type(dt(1,40)) function fun1a(this,dl1,dk1)
    class(base(1)), intent(in) :: this
    class(dl(*,*)), intent(in) :: dl1
    class(dk(1,4)), intent(in) :: dk1
    integer :: i
    fun1a%ival = this%ival - dk1%ival1
    select type(this)
    type is (dt(1,*))
      write(fun1a%chval,*) trim(this%chval), ("!", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class is (base(1))
      write(fun1a%chval,*) "base", ("!", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class default
      stop 15
    end select
  end function fun1a

  type(dt(2,40)) function fun2a(this,dk1,dl1)
    class(base(2)), intent(in) :: this
    class(dk(2,4)), intent(in) :: dk1
    class(dl(*,*)), intent(in) :: dl1
    integer :: i
    fun2a%ival = int(this%ival / dk1%rval, 2)
    select type(this)
    type is (dt(2,*))
      write(fun2a%chval,*) trim(this%chval), ("@", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class is (base(2))
      write(fun2a%chval,*) "base", ("@", trim(dl1%charr(i)), i=1,size(dl1%charr))
    class default
      stop 16
    end select
  end function fun2a


  subroutine display(this, exp)
    class(*), intent(in) :: this
    integer, intent(in) :: exp
    integer :: found
    select type (this)
    type is (dt(1,*)); print *, "DT1:", this%ival, this%l, ">", trim(this%chval), "<"; found = 1
    type is (dt(2,*)); print *, "DT2:", this%ival, this%l, ">", trim(this%chval), "<"; found = 2
    type is (dt(4,*)); print *, "DT4:", this%ival, this%l, ">", trim(this%chval), "<"; found = 4

    type is (dk(1,4)); print *, "DK1:", this%ival1, this%ival2, this%rval; found = 11
    type is (dk(2,4)); print *, "DK2:", this%ival1, this%ival2, this%rval; found = 12
    type is (dk(4,1)); print *, "DK4:", this%ival1, this%ival2, this%rval; found = 14

    type is (dl(*,*)); print *, "DL: ", this%l1, this%l2, ">", this%charr, "<"; found = 0

    class default;     print *, "Unkown type"
                       stop 2
    end select
    if (exp /= found) then
       print *, "Wrong kind. Expected", exp, "found", found
       stop 3
    end if
  end subroutine display

end module dtpPPCDummyPassDTPInterfaceKLFunDTPInhmod


program dtpPPCDummyPassDTPInterfaceKLFunDTPInh

  use dtpPPCDummyPassDTPInterfaceKLFunDTPInhmod
  implicit none
  type(base(1)) :: b1_h
  type(base(2)) :: b2_h
  type(base(4)) :: b4_h
  class(base(4)), pointer :: b4p_h

  type(dt(1,0)) :: t1a_h, t1b_h
  type(dt(2,4)) :: t2a_h, t2b_h
  type(dt(1,3)) :: t1c_h, t1d_h
  type(dt(1,:)) :: t1p_h
  type(dt(4,:)) :: t4p_h

  type(dk(1,4)) :: dk1_h
  type(dk(2,4)) :: dk2_h
  type(dk(4,1)) :: dk4_h
  type(dl(3,5)) :: dl1_h
  type(dl(1,2)) :: dl2_h

  target  :: t1c_h
  pointer :: t1p_h, t4p_h

  b1_h = base(1)(109_1,fun1,fun2,fun4)
  b2_h = base(2)(12112_2,fun1,fun2,fun4)
  b4_h = base(4)(224466881_4,fun1,fun2,fun4)
  allocate(b4p_h, source=base(4)(98765432,fun1,fun2,fun4))

  t1a_h = dt(1,0)(127,fun1,fun2,fun4,"")
  t1b_h = dt(1,0)(-127, fun1a,fun2a,fun4,"")
  t2a_h = dt(2,4)(32000,fun1,fun2,fun4,"abcd")
  t2b_h = dt(2,4)(-12345,fun1a,fun2a,fun4,"efgh")
  t1c_h = dt(1,3)(1,fun1,fun2,fun4,"ijk")
  t1d_h = dt(1,3)(-1,fun1a,fun2a,fun4,"lmn")
  t1p_h => t1c_h
  allocate(t4p_h, source=dt(4,5)(123454321,fun1,fun2,fun4,"opqrs"))

  dk1_h = dk(1,4)(12_1,123,1.23)
  dk2_h = dk(2,4)(123_2,1234,2.345)
  dk4_h = dk(4,1)(1234_4,12_1,3.456)

  dl1_h = dl(3,5)(["abc","def","ghi","jkl","mno"])
  dl2_h = dl(1,2)(["p","q"])

  call runtest1(b1_h, b2_h, b4_h, b4p_h, t1a_h, t1b_h, t1c_h, t1d_h, t1p_h, t2a_h, t2b_h, t4p_h)
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

  b4p_h => t4p_h

  allocate(t1p_h, source=dt(1,5)(0,fun1a,fun2a,fun4,"opqrs"))

  b1_h%p1 => fun1a
  b1_h%p2 => fun2a
  b2_h%p1 => fun1a
  b2_h%p2 => fun2a
  b4_h%p1 => fun1a
  b4_h%p2 => fun2a

  allocate(b4p_h, source=base(4)(98765432,fun1,fun2,fun4))

  call runtest2(b1_h, b2_h, b4_h, b4p_h, t1a_h, t1b_h, t1c_h, t1d_h, t1p_h, t2a_h, t2b_h, t4p_h)

  print *, "done"

contains

  subroutine runtest1(b1, b2, b4, b4p, t1a, t1b, t1c, t1d, t1p, t2a, t2b, t4p)
    implicit none
    type(base(1)) :: b1
    type(base(2)) :: b2
    type(base(4)) :: b4
    class(base(4)), pointer :: b4p

    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(1,3)) :: t1c, t1d
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    target  :: t1c
    pointer :: t1p, t4p

    call display(b1 % p1(dl1_h,dk1_h), 1)
    call display(b2 % p2(dk2_h,dl1_h), 2)
    call display(b4 % p4(dl1_h,dl2_h,dk4_h), 4)
    call display(b4p% p4(dl2_h,dl1_h,dk4_h), 4)
    print *

    call display(t1a % p1(dl1_h,dk1_h), 1)
    call display(t1b % p1(dl2_h,dk1_h), 1)
    call display(t1c % p1(dl1_h,dk1_h), 1)
    call display(t1d % p1(dl2_h,dk1_h), 1)
    call display(t1p % p1(dl1_h,dk1_h), 1)
    call display(t2a % p2(dk2_h,dl1_h), 2)
    call display(t2b % p2(dk2_h,dl2_h), 2)
    call display(t4p % p4(dl1_h,dl2_h,dk(4,1)(111,111_1,1.11)), 4)
    print *

  end subroutine runtest1


  subroutine runtest2(b1, b2, b4, b4p, t1a, t1b, t1c, t1d, t1p, t2a, t2b, t4p)
    implicit none
    type(base(1)) :: b1
    type(base(2)) :: b2
    type(base(4)) :: b4
    class(base(4)), pointer :: b4p

    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(1,3)) :: t1c, t1d
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    target  :: t1c
    pointer :: t1p, t4p
    type(dt(1,40)):: t1res
    type(dt(2,40)):: t2res
    type(dt(4,40)):: t4res

    t1res = b1 % p1(dl1_h,dk1_h);                           print *, t1res%ival, trim(t1res%chval)
    t2res = b2 % p2(dk2_h,dl1_h);                           print *, t2res%ival, trim(t2res%chval)
    t4res = b4 % p4(dl1_h,dl2_h,dk4_h);                     print *, t4res%ival, trim(t4res%chval)
    t4res = b4p% p4(dl2_h,dl1_h,dk4_h);                     print *, t4res%ival, trim(t4res%chval)
    print *

    t1res = t1a % p1(dl1_h,dk1_h);                          print *, t1res%ival, trim(t1res%chval)
    t1res = t1b % p1(dl2_h,dk1_h);                          print *, t1res%ival, trim(t1res%chval)
    t1res = t1c % p1(dl1_h,dk1_h);                          print *, t1res%ival, trim(t1res%chval)
    t1res = t1d % p1(dl2_h,dk1_h);                          print *, t1res%ival, trim(t1res%chval)
    t1res = t1p % p1(dl1_h,dk1_h);                          print *, t1res%ival, trim(t1res%chval)
    t2res = t2a % p2(dk2_h,dl1_h);                          print *, t2res%ival, trim(t2res%chval)
    t2res = t2b % p2(dk2_h,dl2_h);                          print *, t2res%ival, trim(t2res%chval)
    t4res = t4p % p4(dl1_h,dl2_h,dk(4,1)(111,111_1,1.11));  print *, t4res%ival, trim(t4res%chval)
    print *

  end subroutine runtest2

end program dtpPPCDummyPassDTPInterfaceKLFunDTPInh
