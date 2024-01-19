!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to nopass function but DTP arg and result w/ K+L parameters, implied DT interface, using inheritance, ref via module var
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCModNPassDTPInterfaceKLFunDTP (<-dtpPPCLocalNPassDTPInterfaceKLFunDTP<-dtpPPCLocalNPassDTPInterfaceKLFunIntr<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
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
!*  Uses module variables to reference objects.  The arg which is passed in has
!*  kind and len parameters.  The interface is implicit, given as a derived type.
!*  This test also uses inheritance in connection with the return type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCModNPassDTPImplDTKLFunDTPInhmod

  implicit none


  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (type(dt(k,3))), pointer, nopass    :: pDT => null()
     procedure (type(dr(k,3,4))), pointer, nopass  :: pDR => null()
     procedure (type(dl(k,3,2))), pointer, nopass  :: pDL => null()
  end type dt

  type, extends(dt) :: dr (k2)
     integer, kind :: k2
     real(k2) :: rval
  end type dr

  type, extends(dt) :: dl(l2)
     integer, len  :: l2
     integer(k) :: iarr(l,l2)
  end type dl


  type(dt(1,1)) :: ta
  type(dt(1,7)) :: tb
  type(dt(1,:)) :: tp

  type(dr(1,5,4)) :: ra, rb
  type(dr(1,:,4)) :: rp

  type(dl(1,2,3)) :: la, lb
  type(dl(1,:,:)) :: lp

  class(dt(1,:))  :: cp

  target  :: tb, rb, lb
  pointer :: tp, rp, lp, cp

  save :: ta, tb
  save :: ra, rb
  save :: la, lb


contains


  type(dt(1,3)) function fun1a(this)
    class(dt(1,*)), intent(in) :: this
    fun1a % ival = this % ival
    fun1a % chval = this%chval // 'XYZ'
    fun1a % pDT => fun1b
    if (associated(this%pDT)) fun1a % pDT => this%pDT
    fun1a % pDR => fun2b
    if (associated(this%pDR)) fun1a % pDR => this%pDR
    fun1a % pDL => fun3b
    if (associated(this%pDL)) fun1a % pDL => this%pDL
  end function fun1a

  type(dt(1,3)) function fun1b(this)
    class(dt(1,*)), intent(in) :: this
    fun1b % ival  = -this%ival
    fun1b % chval = this%chval // 'ZYX'
    fun1b % pDT => fun1a
    if (associated(this%pDT)) fun1b % pDT => this%pDT
    fun1b % pDR => fun2a
    if (associated(this%pDR)) fun1b % pDR => this%pDR
    fun1b % pDL => fun3a
    if (associated(this%pDL)) fun1b % pDL => this%pDL
  end function fun1b


  type(dr(1,3,4)) function fun2a(this,that)
    class(dt(1,*)), intent(in) :: this
    class(dr(1,*,4)), intent(in) :: that
    fun2a % ival  = this%ival
    fun2a % chval = this%chval // 'ABC'
    fun2a % rval  = that%rval
    fun2a % pDT => fun2b
    if (associated(this%pDT)) fun2a % pDT => this%pDT
    fun2a % pDR => fun2b
    if (associated(this%pDR)) fun2a % pDR => this%pDR
    fun2a % pDL => fun3b
    if (associated(this%pDL)) fun2a % pDL => this%pDL

  end function fun2a

  type(dr(1,3,4)) function fun2b(this,that)
    class(dt(1,*)), intent(in) :: this
    class(dr(1,*,4)), intent(in) :: that
    fun2b % ival  = -this%ival
    fun2b % chval = this%chval // 'DEF'
    fun2b % rval  = -that%rval
    fun2b % pDT => fun1a
    if (associated(this%pDT)) fun2b % pDT => this%pDT
    fun2b % pDR => fun2a
    if (associated(this%pDR)) fun2b % pDR => this%pDR
    fun2b % pDL => fun3a
    if (associated(this%pDL)) fun2b % pDL => this%pDL
  end function fun2b


  type(dl(1,3,2)) function fun3a(this,that)
    class(dt(1,*)), intent(in) :: this
    class(dl(1,*,*)), intent(in) :: that
    fun3a % ival  = this%ival
    fun3a % chval = this%chval // 'GHI'
    fun3a % iarr  = reshape(that%iarr,[3,4],pad=[0_1,0_1,0_1])
    fun3a % pDT => fun1a
    if (associated(this%pDT)) fun3a % pDT => this%pDT
    fun3a % pDR => fun2a
    if (associated(this%pDR)) fun3a % pDR => this%pDR
    fun3a % pDL => fun3a
    if (associated(this%pDL)) fun3a % pDL => this%pDL
  end function fun3a

  type(dl(1,3,2)) function fun3b(this,that)
    class(dt(1,*)), intent(in) :: this
    class(dl(1,*,*)), intent(in) :: that
    fun3b % ival  = -this%ival
    fun3b % chval = this%chval // 'JKL'
    fun3b % iarr  = -reshape(that%iarr,[3,4],pad=[0_1,0_1,0_1])
    fun3b % pDT => fun1a
    if (associated(this%pDT)) fun3b % pDT => this%pDT
    fun3b % pDR => fun2a
    if (associated(this%pDR)) fun3b % pDR => this%pDR
    fun3b % pDL => fun3a
    if (associated(this%pDL)) fun3b % pDL => this%pDL
  end function fun3b




  subroutine display(this, exp)
    class(*), intent(in) :: this
    integer, intent(in) :: exp
    integer :: found
    select type (this)
    type is (dt(1,*)); print *, "DT1:", this%ival, trim(this%chval); found = 1
    type is (dt(2,*)); print *, "DT2:", this%ival, trim(this%chval); found = 2
    type is (dt(4,*)); print *, "DT4:", this%ival, trim(this%chval); found = 4

    type is (dr(1,*,4)); print *, "DR1:", this%ival, this%l, trim(this%chval), this%rval; found = 14
    type is (dr(2,*,4)); print *, "DR2:", this%ival, this%l, trim(this%chval), this%rval; found = 24
    type is (dr(4,*,4)); print *, "DR4:", this%ival, this%l, trim(this%chval), this%rval; found = 44

    type is (dl(1,*,*)); print *, "DL1:", this%ival, this%l, trim(this%chval), this%l2, this%iarr; found = 10
    type is (dl(2,*,*)); print *, "DL2:", this%ival, this%l, trim(this%chval), this%l2, this%iarr; found = 20
    type is (dl(4,*,*)); print *, "DL4:", this%ival, this%l, trim(this%chval), this%l2, this%iarr; found = 40

    class default;     print *, "Unkown type"
                       stop 2
    end select
    if (exp /= found) then
       print *, "Wrong kind. Expected", exp, "found", found
       stop 3
    end if
  end subroutine display

end module dtpPPCModNPassDTPImplDTKLFunDTPInhmod


program dtpPPCModNPassDTPImplDTKLFunDTPInh

  use dtpPPCModNPassDTPImplDTKLFunDTPInhmod
  implicit none
  type(dt(1,3))  :: tres
  type(dr(1,3,4)):: rres
  type(dl(1,3,2)):: lres
  integer :: i

  ta = dt(1,1)("",127,pDT=fun1a,pDR=fun2a,pDL=fun3a)
  tb = dt(1,7)("alberta",-127,pDT=fun1b,pDR=fun2b,pDL=fun3b)
  ra = dr(1,5,4)("abcd",32000,rval=1.1e10,pDT=fun1a,pDR=fun2a,pDL=fun3a)
  rb = dr(1,5,4)("efgh",-12345,rval=2.2e-2,pDT=fun1b,pDR=fun2b,pDL=fun3b)
  la = dl(1,2,3)("ijk",1,iarr=reshape([(i,i=1,6)],[2,3]),pDT=fun1a,pDR=fun2a,pDL=fun3a)
  lb = dl(1,2,3)("lmn",-1,iarr=reshape([(i**2,i=1,6)],[2,3]),pDT=fun1b,pDR=fun2b,pDL=fun3b)
  tp => tb
  rp => rb
  lp => lb

  call display(fun1a(ta), 1)
  call display(fun1a(tb), 1)
  call display(fun1b(ta), 1)
  call display(fun1b(tb), 1)
  call display(fun1b(tp), 1)
  print *

  call display(fun2a(ta,ra), 14)
  call display(fun2a(tb,ra), 14)
  call display(fun2b(ta,rb), 14)
  call display(fun2b(tb,rb), 14)
  call display(fun2b(tp,rp), 14)
  print *

  call display(fun3a(ta,la), 10)
  call display(fun3a(tb,la), 10)
  call display(fun3b(ta,lb), 10)
  call display(fun3b(tb,lb), 10)
  call display(fun3b(tp,lp), 10)
  print *

  tres = fun1a(ta);      print *, tres % ival, trim(tres%chval)
  tres = fun1a(tb);      print *, tres % ival, trim(tres%chval)
  tres = fun1b(ta);      print *, tres % ival, trim(tres%chval)
  tres = fun1b(tb);      print *, tres % ival, trim(tres%chval)
  tres = fun1b(tp);      print *, tres % ival, trim(tres%chval)
  print *

  rres = fun2a(ta,ra);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = fun2a(tb,ra);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = fun2b(ta,rb);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = fun2b(tb,rb);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = fun2b(tp,rp);   print *, rres % ival, trim(rres%chval), rres%rval
  print *

  lres = fun3a(ta,la);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = fun3a(tb,la);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = fun3b(ta,lb);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = fun3b(tb,lb);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = fun3b(tp,lp);   print *, lres % ival, trim(lres%chval), lres%iarr
  print *

  call display(ta % pDT(ta), 1)
  call display(ta % pDT(tb), 1)
  call display(tb % pDT(ta), 1)
  call display(tb % pDT(tb), 1)
  call display(tb % pDT(tp), 1)
  print *

  call display(ta % pDR(ta,ra), 14)
  call display(ta % pDR(tb,ra), 14)
  call display(tb % pDR(ta,rb), 14)
  call display(tb % pDR(tb,rb), 14)
  call display(tb % pDR(tp,rp), 14)
  print *

  call display(ta % pDL(ta,la), 10)
  call display(ta % pDL(tb,la), 10)
  call display(tb % pDL(ta,lb), 10)
  call display(tb % pDL(tb,lb), 10)
  call display(tb % pDL(tp,lp), 10)
  print *

  tres = ta % pDT(ta);      print *, tres % ival, trim(tres%chval)
  tres = ta % pDT(tb);      print *, tres % ival, trim(tres%chval)
  tres = tb % pDT(ta);      print *, tres % ival, trim(tres%chval)
  tres = tb % pDT(tb);      print *, tres % ival, trim(tres%chval)
  tres = tb % pDT(tp);      print *, tres % ival, trim(tres%chval)
  print *

  rres = ta % pDR(ta,ra);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = ta % pDR(tb,ra);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = tb % pDR(ta,rb);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = tb % pDR(tb,rb);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = tb % pDR(tp,rp);   print *, rres % ival, trim(rres%chval), rres%rval
  print *

  lres = ta % pDT(ta,la);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = ta % pDT(tb,la);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = tb % pDT(ta,lb);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = tb % pDT(tb,lb);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = tb % pDT(tp,lp);   print *, lres % ival, trim(lres%chval), lres%iarr
  print *


  ta % pDT => fun1b
  ta % pDR => fun2b
  ta % pDL => fun3b

  tb % pDT => fun1a
  tb % pDR => fun2a
  tb % pDL => fun3a

  ra % pDT => fun1b
  ra % pDR => fun2b
  ra % pDL => fun3b

  rb % pDT => fun1a
  rb % pDR => fun2a
  rb % pDL => fun3a

  la % pDT => fun1b
  la % pDR => fun2b
  la % pDL => fun3b

  lb % pDT => fun1a
  lb % pDR => fun2a
  lb % pDL => fun3a

  call display(ta % pDT(ta), 1)
  call display(ta % pDT(tb), 1)
  call display(tb % pDT(ta), 1)
  call display(tb % pDT(tb), 1)
  call display(tb % pDT(tp), 1)
  print *

  call display(ta % pDR(ta,ra), 14)
  call display(ta % pDR(tb,ra), 14)
  call display(tb % pDR(ta,rb), 14)
  call display(tb % pDR(tb,rb), 14)
  call display(tb % pDR(tp,rp), 14)
  print *

  call display(ta % pDL(ta,la), 10)
  call display(ta % pDL(tb,la), 10)
  call display(tb % pDL(ta,lb), 10)
  call display(tb % pDL(tb,lb), 10)
  call display(tb % pDL(tp,lp), 10)
  print *

  tres = ta % pDT(ta);      print *, tres % ival, trim(tres%chval)
  tres = ta % pDT(tb);      print *, tres % ival, trim(tres%chval)
  tres = tb % pDT(ta);      print *, tres % ival, trim(tres%chval)
  tres = tb % pDT(tb);      print *, tres % ival, trim(tres%chval)
  tres = tb % pDT(tp);      print *, tres % ival, trim(tres%chval)
  print *

  rres = ta % pDR(ta,ra);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = ta % pDR(tb,ra);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = tb % pDR(ta,rb);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = tb % pDR(tb,rb);   print *, rres % ival, trim(rres%chval), rres%rval
  rres = tb % pDR(tp,rp);   print *, rres % ival, trim(rres%chval), rres%rval
  print *

  lres = ta % pDT(ta,la);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = ta % pDT(tb,la);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = tb % pDT(ta,lb);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = tb % pDT(tb,lb);   print *, lres % ival, trim(lres%chval), lres%iarr
  lres = tb % pDT(tp,lp);   print *, lres % ival, trim(lres%chval), lres%iarr
  print *

  allocate(cp, source=dt(1,5)("opqrs",0,fun1b,fun2b,fun3b))

  call display(cp % pDT(ta), 1)
  call display(cp % pDR(ta,ra), 14)
  call display(cp % pDL(ta,la), 10)
  print *

  call display(tres % pDT(tres), 1)
  call display(rres % pDR(tres,rres), 1)
  call display(lres % pDL(tres,lres), 1)
  print *

  print *, "done"

end program dtpPPCModNPassDTPImplDTKLFunDTPInh
