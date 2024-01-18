!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to nopass function, implicit interface (derived type) with intrinsic dummy args, K+L, DTP result, ref via dummy arg
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
!*  Create procedure pointers which are references to nopass functions which
!*  return a value of derived type and expect mixed dummy arguments.  Define a
!*  parameterised derived type with procedure pointers and create instances of
!*  those types, initialising them with a structure constructor containing a
!*  reference to one of a pair of routines.  Invoke the referenced procedure via
!*  the pointer several times, and then assign new procedure references and
!*  repeat.  Uses dummy args to reference objects.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyNPassIntrImplDTKLFunDTPmod

  implicit none

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

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (type(dk(k,8))), pointer, nopass :: p1 => null()
     procedure (fun2c), pointer, nopass :: p2 => null()
     procedure (type(dt(k,2))), pointer, nopass :: p3 => null()
     procedure (fun2i), pointer, nopass :: p2i => null()
  end type dt


contains


  type(dk(1,8)) function fun1_1(i1,i2,r)
    integer(1), intent(in) :: i1
    integer(8), intent(in) :: i2
    real(8), intent(in)    :: r
    fun1_1 = dk(1,8)(i1+1,i2+1,r+1.11)
  end function fun1_1

  type(dk(1,8)) function fun1_1a(r,i1,i2)
    integer(1), intent(in) :: i1
    integer(8), intent(in) :: i2
    real(8), intent(in)    :: r
    fun1_1a = dk(1,8)(i1+10,i2+10,r+11.11)
  end function fun1_1a

  type(dk(2,8)) function fun1_2(i1,i2,r)
    integer(2), intent(in) :: i1
    integer(8), intent(in) :: i2
    real(8), intent(in)    :: r
    fun1_2 = dk(2,8)(i1+2,i2+2,r+2.21)
  end function fun1_2

  type(dk(2,8)) function fun1_2a(r,i1,i2)
    integer(2), intent(in) :: i1
    integer(8), intent(in) :: i2
    real(8), intent(in)    :: r
    fun1_2a = dk(2,8)(i1+20,i2+20,r+22.21)
  end function fun1_2a

  type(dk(4,8)) function fun1_4(i1,i2,r)
    integer(4), intent(in) :: i1
    integer(8), intent(in) :: i2
    real(8), intent(in)    :: r
    fun1_4 = dk(4,8)(i1+4,i2+4,r+4.41)
  end function fun1_4

  type(dl(3,4)) function fun2c(carr)
    character(*), intent(in) :: carr(:)
    fun2c%charr = ''
    fun2c%charr(1:min(fun2c%l2,size(carr))) = carr
  end function fun2c

  type(dl(3,4)) function fun2i(iarr)
    integer, intent(in) :: iarr(:)
    integer :: i
    fun2i%charr = ''
    do i = 1,min(fun2i%l2,size(iarr))
       write(fun2i%charr(i),*) iarr(i)
    end do
  end function fun2i


  type(dt(1,2)) function fun3_1(i,ch)
    integer(1), intent(in)  :: i
    character(*), intent(in) :: ch
    fun3_1 % ival = i + 1
    fun3_1 % chval = ch
    fun3_1 % p1 => fun1_1
    fun3_1 % p2 => fun2c
    fun3_1 % p2i => fun2i
    fun3_1 % p3 => fun3_1
  end function fun3_1

  type(dt(2,2)) function fun3_2(i,ch)
    integer(2), intent(in)  :: i
    character(*), intent(in) :: ch
    fun3_2 % ival = i + 2
    fun3_2 % chval = ch
    fun3_2 % p1 => fun1_2
    fun3_2 % p2i => fun2i
    fun3_2 % p2 => fun2c
    fun3_2 % p3 => fun3_2
  end function fun3_2

  type(dt(4,2)) function fun3_4(i,ch)
    integer(4), intent(in)  :: i
    character(*), intent(in) :: ch
    fun3_4 % ival = i + 4
    fun3_4 % chval = ch
    fun3_4 % p1 => fun1_4
    fun3_4 % p2i => fun2i
    fun3_4 % p2 => fun2c
    fun3_4 % p3 => fun3_4
  end function fun3_4


  subroutine display(this, exp)
    class(*), intent(in) :: this
    integer, intent(in) :: exp
    integer :: found
    select type (this)
    type is (dl(*,*)); print *, "DL:", this%l1, this%l2, len(this%charr), size(this%charr), ">", this%charr, "<"; found = 0
    type is (dt(1,*)); print *, "DT1:", this%k, kind(this%ival), this%ival, trim(this%chval); found = 1
    type is (dt(2,*)); print *, "DT2:", this%k, kind(this%ival), this%ival, trim(this%chval); found = 2
    type is (dt(4,*)); print *, "DT4:", this%k, kind(this%ival), this%ival, trim(this%chval); found = 4
    type is (dk(1,8)); print *, "DK18:", this%k1, this%k2, kind(this%ival1), this%ival1, kind(this%ival2), this%ival2, kind(this%rval), this%rval; found = 18
    type is (dk(2,8)); print *, "DK28:", this%k1, this%k2, kind(this%ival1), this%ival1, kind(this%ival2), this%ival2, kind(this%rval), this%rval; found = 28
    type is (dk(4,8)); print *, "DK48:", this%k1, this%k2, kind(this%ival1), this%ival1, kind(this%ival2), this%ival2, kind(this%rval), this%rval; found = 48

    class default;     print *, "Unkown type"
                       stop 2
    end select
    if (exp /= found) then
       print *, "Wrong kind. Expected", exp, "found", found
       stop 3
    end if
  end subroutine display

end module dtpPPCDummyNPassIntrImplDTKLFunDTPmod


program dtpPPCDummyNPassIntrImplDTKLFunDTP

  use dtpPPCDummyNPassIntrImplDTKLFunDTPmod
  implicit none
  type(dt(1,0)) :: t1a_h, t1b_h
  type(dt(2,4)) :: t2a_h, t2b_h
  type(dt(4,3)) :: t4a_h, t4b_h
  type(dt(1,:)) :: t1p_h
  type(dt(4,:)) :: t4p_h
  target  :: t1a_h, t4a_h
  pointer :: t1p_h, t4p_h

  t1a_h = dt(1,0)('',127,fun1_1,fun2c,fun3_1, fun2i)
  t1b_h = dt(1,0)('',-127,fun1_1a,fun2c,fun3_1, fun2i)
  t2a_h = dt(2,4)("abcd",32000,fun1_2,fun2c,fun3_2, fun2i)
  t2b_h = dt(2,4)("efgh",-12345,fun1_2a,fun2c,fun3_2, fun2i)
  t4a_h = dt(4,3)("ijk",1,fun1_4,fun2c,fun3_4,fun2i)
  t4b_h = dt(4,3)("lmn",-1,fun1_4,fun2c,fun3_4,fun2i)
  t1p_h => t1a_h
  t4p_h => t4a_h

  ! Verify that the functions do what they are supposed to (minus the procedure pointer use):
  call display(fun1_1(-1_1,-101_8,-1001.1_8), 18)
  call display(fun1_2(-2_2,-102_8,-1002.1_8), 28)
  call display(fun1_4(-4_4,-104_8,-1004.1_8), 48)
  call display(fun2i([1,2,3,4]), 0)
  call display(fun2c(["abc","def","ghi","jkl"]), 0)
  call display(fun3_1(-1_1,"abc"), 1)
  call display(fun3_2(-2_2,"def"), 2)
  call display(fun3_4(-4_4,"ghi"), 4)
  print *

  call runtest1(t1a_h, t1b_h, t1p_h, t2a_h, t2b_h, t4a_h, t4b_h, t4p_h)
  t1a_h % p1 => fun1_1a
  t1a_h % p2 => fun2c
  t1a_h % p2i => fun2i

  t1b_h % p1 => fun1_1
  t1b_h % p2i => fun2i
  t1b_h % p2 => fun2c

  t2a_h % p1 => fun1_1a
  t2a_h % p2 => fun2c
  t2a_h % p2i => fun2i

  t2b_h % p1 => fun1_1
  t2b_h % p2i => fun2i
  t2b_h % p2 => fun2c

  t4a_h % p2 => fun2c
  t4a_h % p2i => fun2i

  t4b_h % p1 => fun1_4
  t4b_h % p2i => fun2i
  t4b_h % p2 => fun2c

  allocate(t1p_h, source=dt(1,5)("opqrs",0,fun1_1,fun2c,fun3_1,fun2i))
  allocate(t4p_h, source=dt(4,5)("opqrs",123454321,fun1_4,fun2c,fun3_4,fun2i))

  call runtest2(t1a_h, t1b_h, t1p_h, t2a_h, t2b_h, t4a_h, t4b_h, t4p_h)

  print *, "done"

contains

  subroutine runtest1(t1a, t1b, t1p, t2a, t2b, t4a, t4b, t4p)
    implicit none
    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(4,3)) :: t4a, t4b
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    pointer :: t1p, t4p

    call display(t1a % p1(1_1,101_8,1001.1_8), 18)
    call display(t1b % p1(1002.1_8,2_1,102_8), 18)
    call display(t1p % p1(3_1,103_8,1003.1_8), 18)
    call display(t2a % p1(4_2,104_8,1004.1_8), 28)
    call display(t2b % p1(1005.1_8,5_2,105_8), 28)
    call display(t4a % p1(6_4,106_8,1006.1_8), 48)
    call display(t4b % p1(7_4,107_8,1007.1_8), 48)
    call display(t4p % p1(8_4,108_8,1008.1_8), 48)
    print *

    call display(t1a % p2i([1,2,3,4]), 0)
    call display(t1b % p2(['abc','def','ghi','jkl']), 0)
    call display(t1p % p2i([0]), 0)
    call display(t2a % p2i([11,12,13,14,15]), 0)
    call display(t2b % p2(['abcd','defg','hijk','lmno','pqrs']), 0)
    call display(t4a % p2i([-1,9]), 0)
    call display(t4b % p2(['ab','de','hi']), 0)
    call display(t4p % p2i([integer::]), 0)
    print *

    call display(t1a % p3(1_1,'ab'), 1)
    call display(t1b % p3(2_1,'jkl'), 1)
    call display(t1p % p3(3_1,'mnopqr'), 1)
    call display(t2a % p3(4_2,''), 2)
    call display(t2b % p3(5_2,'yy'), 2)
    call display(t4a % p3(6_4,'xx'), 4)
    call display(t4b % p3(7_4,'abdehi'), 4)
    call display(t4p % p3(8_4,'integer'), 4)
    print *

  end subroutine runtest1


  subroutine runtest2(t1a, t1b, t1p, t2a, t2b, t4a, t4b, t4p)
    implicit none
    type(dt(1,0)) :: t1a, t1b
    type(dt(2,4)) :: t2a, t2b
    type(dt(4,3)) :: t4a, t4b
    type(dt(1,:)) :: t1p
    type(dt(4,:)) :: t4p
    pointer :: t1p, t4p
    type(dl(3,4)):: lres
    type(dk(1,8)):: k1res
    type(dk(2,8)):: k2res
    type(dk(4,8)):: k4res
    type(dt(1,2)):: t1res
    type(dt(2,2)):: t2res
    type(dt(4,2)):: t4res


    k1res = t1a % p1(1_1,101_8,1001.1_8);                     print *, k1res%ival1, k1res%ival2, k1res%rval
    k1res = t1b % p1(1002.1_8,2_1,102_8);                     print *, k1res%ival1, k1res%ival2, k1res%rval
    k1res = t1p % p1(3_1,103_8,1003.1_8);                     print *, k1res%ival1, k1res%ival2, k1res%rval
    k2res = t2a % p1(4_2,104_8,1004.1_8);                     print *, k2res%ival1, k2res%ival2, k2res%rval
    k2res = t2b % p1(1005.1_8,5_2,105_8);                     print *, k2res%ival1, k2res%ival2, k2res%rval
    k4res = t4a % p1(6_4,106_8,1006.1_8);                     print *, k4res%ival1, k4res%ival2, k4res%rval
    k4res = t4b % p1(7_4,107_8,1007.1_8);                     print *, k4res%ival1, k4res%ival2, k4res%rval
    k4res = t4p % p1(8_4,108_8,1008.1_8);                     print *, k4res%ival1, k4res%ival2, k4res%rval
    print *

    lres = t1a % p2i([1,2,3,4]);                               print *, ">", lres%charr, "<"
    lres = t1b % p2(['abc','def','ghi','jkl']);               print *, ">", lres%charr, "<"
    lres = t1p % p2i([0]);                                     print *, ">", lres%charr, "<"
    lres = t2a % p2i([11,12,13,14,15]);                        print *, ">", lres%charr, "<"
    lres = t2b % p2(['abcd','defg','hijk','lmno','pqrs']);    print *, ">", lres%charr, "<"
    lres = t4a % p2i([-1,9]);                                  print *, ">", lres%charr, "<"
    lres = t4b % p2(['ab','de','hi']);                        print *, ">", lres%charr, "<"
    lres = t4p % p2i([integer::]);                             print *, ">", lres%charr, "<"
    print *

    t1res = t1a % p3(1_1,'ab');                                 print *, t1res%ival, ">", t1res%chval, "<"
    t1res = t1b % p3(2_1,'jkl');                                print *, t1res%ival, ">", t1res%chval, "<"
    t1res = t1p % p3(3_1,'mnopqr');                             print *, t1res%ival, ">", t1res%chval, "<"
    t2res = t2a % p3(4_2,'');                                   print *, t2res%ival, ">", t2res%chval, "<"
    t2res = t2b % p3(5_2,'yy');                                 print *, t2res%ival, ">", t2res%chval, "<"
    t4res = t4a % p3(6_4,'xx');                                 print *, t4res%ival, ">", t4res%chval, "<"
    t4res = t4b % p3(7_4,'abdehi');                             print *, t4res%ival, ">", t4res%chval, "<"
    t4res = t4p % p3(8_4,'integer');                            print *, t4res%ival, ">", t4res%chval, "<"
    print *

  end subroutine runtest2

end program dtpPPCDummyNPassIntrImplDTKLFunDTP
