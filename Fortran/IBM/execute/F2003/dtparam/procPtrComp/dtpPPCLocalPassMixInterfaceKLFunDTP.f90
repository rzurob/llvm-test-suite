!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCLocalPassMixInterfaceKLFunDTP
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to pass function with mixed dummy args, K+L, DTP result
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalNPassDTPInterfaceKLFunDTP (<-dtpPPCLocalNPassDTPInterfaceKLFunIntr<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to pass functions which
!*  return a value of derived type and expect mixed dummy arguments.  Define a
!*  parameterised derived type with procedure pointers and create instances of
!*  those types, initialising them with a structure constructor containing a
!*  reference to one of a pair of routines.  Invoke the referenced procedure via
!*  the pointer several times, and then assign new procedure references and
!*  repeat.  Uses local variables to reference objects.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalPassMixInterfaceKLFunDTPmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (f1), pointer, pass :: p1 => null()
     procedure (f2), pointer, pass :: p2 => null()
     procedure (f4), pointer, pass :: p4 => null()
  end type dt

  abstract interface

     type(dt(1,40)) function f1(d,i,c)
       import :: dt
       class(dt(1,*)), intent(in) :: d
       integer(1), intent(in)  :: i
       character(*), intent(in) :: c
     end function f1

     type(dt(2,40)) function f2(d,i)
       import :: dt
       class(dt(2,*)), intent(in) :: d
       integer(2), intent(in)  :: i
     end function f2

     type(dt(4,40)) function f4(d1,d2)
       import :: dt
       class(dt(4,*)), intent(in) :: d1, d2
     end function f4

  end interface


contains


  type(dt(1,40)) function add(this,i,ch)
    class(dt(1,*)), intent(in) :: this
    integer(1), intent(in)  :: i
    character(*), intent(in) :: ch
    add % ival = this % ival + i
    write(add % chval,*) trim(this%chval), "/", trim(ch)
  end function add

  type(dt(2,40)) function dup(this,factor)
    class(dt(2,*)), intent(in) :: this
    integer(2), intent(in)  :: factor
    integer :: i
    dup % ival = this % ival * factor
    write(dup % chval,*) (this%chval,i=1,factor)
  end function dup

  type(dt(4,40)) function cat(this,that)
    class(dt(4,*)), intent(in) :: this, that
    cat % ival = this % ival * 10 ** int(log(real(that%ival))+1) + that%ival
    write(cat % chval,*) this%chval, that%chval
  end function cat


  type(dt(1,40)) function sub(this,i,ch)
    class(dt(1,*)), intent(in) :: this
    integer(1), intent(in)  :: i
    character(*), intent(in) :: ch
    integer :: inx
    sub % ival = this % ival - i
    inx = index(this%chval, ch)
    if (inx==0) then
       sub%chval = this%chval
    else
       write(sub % chval,*) this%chval(1:inx-1), this%chval(inx+len(ch):)
    end if
  end function sub

  type(dt(2,40)) function shrink(this,factor)
    class(dt(2,*)), intent(in) :: this
    integer(2), intent(in)  :: factor
    shrink % ival = this % ival / factor
    write(shrink % chval,*) this%chval(1:len(this%chval)/factor)
  end function shrink


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

end module dtpPPCLocalPassMixInterfaceKLFunDTPmod


program dtpPPCLocalPassMixInterfaceKLFunDTP

  use dtpPPCLocalPassMixInterfaceKLFunDTPmod
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

  t1a = dt(1,0)("",127,add,dup,cat)
  t1b = dt(1,0)("",-127,sub,shrink,cat)
  t2a = dt(2,4)("abcd",32000,add,dup,cat)
  t2b = dt(2,4)("efgh",-12345,sub,shrink,cat)
  t1c = dt(1,3)("ijk",1,add,dup,cat)
  t1d = dt(1,3)("lmn",-1,sub,shrink,cat)
  t1p => t1c
  allocate(t4p, source=dt(4,5)("opqrs",123454321,add,dup,cat))

  call display(t1a % p1(-4_1,"abcd"), 1)
  call display(t1b % p1(-7_1,"abcd"), 1)
  call display(t1c % p1(100_1,"lmno"), 1)
  call display(t1d % p1(-99_1,"m"), 1)
  call display(t1p % p1(99_1,"m"), 1)
  call display(t2a % p2(2_2), 2)
  call display(t2b % p2(2_2), 2)
  call display(t4p % p4(dt(4,3)("tuv",2345)), 4)
  print *

  call display(t1a % p1(0_1,""), 1)
  call display(t1b % p1(0_1,""), 1)
  call display(t1c % p1(0_1,""), 1)
  call display(t1d % p1(0_1,""), 1)
  call display(t1p % p1(0_1,""), 1)
  call display(t2a % p2(1_2), 2)
  call display(t2b % p2(1_2), 2)
  call display(t4p % p4(dt(4,0)("",1)), 4)
  print *

  t1a % p1 => sub
  t1a % p2 => shrink

  t1b % p1 => add
  t1b % p2 => dup

  t1c % p1 => sub
  t1c % p2 => shrink

  t1d % p1 => add
  t1d % p2 => dup

  t2a % p1 => sub
  t2a % p2 => shrink

  t2b % p1 => add
  t2b % p2 => dup

  allocate(t1p, source=dt(1,5)("opqrs",0,sub,shrink,cat))

  t1res = t1a % p1(-4_1,"abcd");          print *, t1res%ival, trim(t1res%chval)
  t1res = t1b % p1(-7_1,"abcd");          print *, t1res%ival, trim(t1res%chval)
  t1res = t1c % p1(100_1,"lmno");         print *, t1res%ival, trim(t1res%chval)
  t1res = t1d % p1(-99_1,"m");            print *, t1res%ival, trim(t1res%chval)
  t1res = t1p % p1(99_1,"m");             print *, t1res%ival, trim(t1res%chval)
  t2res = t2a % p2(2_2);                  print *, t2res%ival, trim(t2res%chval)
  t2res = t2b % p2(2_2);                  print *, t2res%ival, trim(t2res%chval)
  t4res = t4p % p4(dt(4,3)("tuv",2345));  print *, t4res%ival, trim(t4res%chval)
  print *

  t1res = t1a % p1(0_1,"");               print *, t1res%ival, trim(t1res%chval)
  t1res = t1b % p1(0_1,"");               print *, t1res%ival, trim(t1res%chval)
  t1res = t1c % p1(0_1,"");               print *, t1res%ival, trim(t1res%chval)
  t1res = t1d % p1(0_1,"");               print *, t1res%ival, trim(t1res%chval)
  t1res = t1p % p1(0_1,"");               print *, t1res%ival, trim(t1res%chval)
  t2res = t2a % p2(1_2);                  print *, t2res%ival, trim(t2res%chval)
  t2res = t2b % p2(1_2);                  print *, t2res%ival, trim(t2res%chval)
  t4res = t4p % p4(dt(4,0)("",1));        print *, t4res%ival, trim(t4res%chval)
  print *

  print *, "done"

end program dtpPPCLocalPassMixInterfaceKLFunDTP
