!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCLocalPassDTPModKFunDTPInh
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-03-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to pass function, DTP arg + result w/ K parameter, ref via local var, interface from module procedure, inheritance
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpPPCModPassDTPInterfaceKFunDTP (<-dtpPPCModNPassDTPInterfaceKLFunDTP<-dtpPPCLocalNPassDTPInterfaceKLFunDTP<-dtpPPCLocalNPassDTPInterfaceKLFunIntr<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to pass functions which return a
!*  value of derived type.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.
!*  The type has a simple kind parameter.  Inheritance is involved.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalPassDTPModKFunDTPInhmod

  implicit none

  type di (k)
     integer, kind :: k
     integer(k)   :: ival
     procedure (fun14), pointer, pass :: p14 => null()
     procedure (fun24), pointer, pass :: p24 => null()
     procedure (fun48), pointer, pass :: p48 => null()
  end type di

  type, extends(di) :: ditto
     integer(k) :: ival2
  end type ditto

  type dr (k)
     integer, kind :: k
     real(k)       :: rval
  end type dr

  type, extends(dr) :: demento
     real(k)       :: rval2
  end type demento

  type dt (k1,k2)
     integer, kind :: k1, k2
     integer(k1)   :: ival
     real(k2)      :: rval
  end type dt


contains

  type(dt(1,4)) function fun14(this, that)
    class(di(1)), intent(in) :: this
    class(dr(4)), intent(in) :: that
    fun14%ival = this%ival
    fun14%rval = that%rval
  end function fun14

  type(dt(2,4)) function fun24(this, that)
    class(di(2)), intent(in) :: this
    class(dr(4)), intent(in) :: that
    fun24%ival = this%ival
    fun24%rval = that%rval
  end function fun24


  type(dt(4,8)) function fun48(this, that)
    class(di(4)), intent(in) :: this
    class(dr(8)), intent(in) :: that
    fun48%ival = this%ival
    fun48%rval = that%rval
  end function fun48


  type(dt(1,4)) function fun14a(this, that)
    class(di(1)), intent(in) :: this
    class(dr(4)), intent(in) :: that
    select type(this)
    type is (ditto(1)); fun14a%ival = this%ival2
    class default;      fun14a%ival = this%ival
    end select
    select type(that)
    type is (demento(4)); fun14a%rval = that%rval2
    class default;        fun14a%rval = that%rval
    end select
  end function fun14a

  type(dt(2,4)) function fun24a(this, that)
    class(di(2)), intent(in) :: this
    class(dr(4)), intent(in) :: that
    select type(this)
    type is (ditto(2)); fun24a%ival = this%ival2
    class default;      fun24a%ival = this%ival
    end select
    select type(that)
    type is (demento(4)); fun24a%rval = that%rval2
    class default;        fun24a%rval = that%rval
    end select
  end function fun24a


  type(dt(4,8)) function fun48a(this, that)
    class(di(4)), intent(in) :: this
    class(dr(8)), intent(in) :: that
    select type(this)
    type is (ditto(4)); fun48a%ival = this%ival2
    class default;      fun48a%ival = this%ival
    end select
    select type(that)
    type is (demento(8)); fun48a%rval = that%rval2
    class default;        fun48a%rval = that%rval
    end select
  end function fun48a


  subroutine display(this, exp)
    class(*), intent(in) :: this
    integer, intent(in) :: exp
    integer :: found
    select type (this)
    type is (dt(1,4)); print *, this%ival, this%rval; found = 1
    type is (dt(2,4)); print *, this%ival, this%rval; found = 2
    type is (dt(4,8)); print *, this%ival, this%rval; found = 4
    class default;     print *, "Unkown type"
                       stop 2
    end select
    if (exp /= found) then
       print *, "Wrong kind. Expected", exp, "found", found
       stop 3
    end if
  end subroutine display

end module dtpPPCLocalPassDTPModKFunDTPInhmod


program dtpPPCLocalPassDTPModKFunDTPInh

  use dtpPPCLocalPassDTPModKFunDTPInhmod
  implicit none
  type(di(1)) :: i1a, i1b, i1p
  type(di(2)) :: i2a, i2b, i2p
  type(di(4)) :: i4a, i4b, i4p

  type(dr(4)) :: r4a, r4b, r4p
  type(dr(8)) :: r8a, r8b, r8p

  type(ditto(1)) :: dit1a, dit1b, dit1p
  type(ditto(2)) :: dit2a, dit2b, dit2p
  type(ditto(4)) :: dit4a, dit4b, dit4p

  type(demento(4)) :: dem4a, dem4b, dem4p
  type(demento(8)) :: dem8a, dem8b, dem8p

  type(dt(1,4)) :: t14a, t14b, t14p
  type(dt(2,4)) :: t24a, t24b, t24p
  type(dt(4,8)) :: t48a, t48b, t48p

  target  :: i1a, i2a, i4a, r4a, r8a, t14a, t24a, t48a, dit1a, dit2a, dit4a, dem4a, dem8a
  pointer :: i1p, i2p, i4p, r4p, r8p, t14p, t24p, t48p, dit1p, dit2p, dit4p, dem4p, dem8p

  i1a = di(1)(127, fun14,fun24,fun48)
  i1b = di(1)(-127, fun14a,fun24a,fun48a)
  i2a = di(2)(32000, fun14,fun24,fun48)
  i2b = di(2)(-1234, fun14a,fun24a,fun48a)
  i4a = di(4)(200032000, fun14,fun24,fun48)
  i4b = di(4)(-12341234, fun14a,fun24a,fun48a)

  r4a = dr(4)(9.876543210987654321)
  r4b = dr(4)(1.234567890123456789)
  r8a = dr(8)(9.876543210987654321_8)
  r8b = dr(8)(1.234567890123456789_8)

  dit1a = ditto(1)(127, fun14,fun24,fun48, 25)
  dit1b = ditto(1)(-127, fun14a,fun24a,fun48a, 66)
  dit2a = ditto(2)(32000, fun14,fun24,fun48, 1700)
  dit2b = ditto(2)(-1234, fun14a,fun24a,fun48a, 7006)
  dit4a = ditto(4)(200032000, fun14,fun24,fun48, 64564209)
  dit4b = ditto(4)(-12341234, fun14a,fun24a,fun48a, 34826079)

  dem4a = demento(4)(9.876543210987654321,   183.119295105978604)
  dem4b = demento(4)(1.234567890123456789,   238.5461787463431)
  dem8a = demento(8)(9.876543210987654321_8, 203.858333039903036_8)
  dem8b = demento(8)(1.234567890123456789_8, 114.29756755939816_8)

  t14a = dt(1,4)(0,0.0)
  t14b = dt(1,4)(0,0.0)
  t24a = dt(2,4)(0,0.0)
  t24b = dt(2,4)(0,0.0)
  t48a = dt(4,8)(0,0.0)
  t48b = dt(4,8)(0,0.0)

  t14a = i1a%p14(r4a); print *, t14a%ival, t14a%rval, i1a%ival, r4a%rval
  t14b = i1b%p14(r4b); print *, t14b%ival, t14b%rval, i1b%ival, r4b%rval
  t24a = i2a%p24(r4a); print *, t24a%ival, t24a%rval, i2a%ival, r4a%rval
  t24b = i2b%p24(r4b); print *, t24b%ival, t24b%rval, i2b%ival, r4b%rval
  t48a = i4a%p48(r8a); print *, t48a%ival, t48a%rval, i4a%ival, r8a%rval
  t48b = i4b%p48(r8b); print *, t48b%ival, t48b%rval, i4b%ival, r8b%rval
  print *

  t14a = dit1a%p14(dem4a); print *, t14a%ival, t14a%rval, dit1a%ival, dit1a%ival2, dem4a%rval, dem4a%rval2
  t14b = dit1b%p14(dem4b); print *, t14b%ival, t14b%rval, dit1b%ival, dit1b%ival2, dem4b%rval, dem4b%rval2
  t24a = dit2a%p24(dem4a); print *, t24a%ival, t24a%rval, dit2a%ival, dit2a%ival2, dem4a%rval, dem4a%rval2
  t24b = dit2b%p24(dem4b); print *, t24b%ival, t24b%rval, dit2b%ival, dit2b%ival2, dem4b%rval, dem4b%rval2
  t48a = dit4a%p48(dem8a); print *, t48a%ival, t48a%rval, dit4a%ival, dit4a%ival2, dem8a%rval, dem8a%rval2
  t48b = dit4b%p48(dem8b); print *, t48b%ival, t48b%rval, dit4b%ival, dit4b%ival2, dem8b%rval, dem8b%rval2
  print *

  i1p  => i1a
  i2p  => i2a
  i4p  => i4a
  r4p  => r4a
  r8p  => r8a
  dit1p=> dit1a
  dit2p=> dit2a
  dit4p=> dit4a
  dem4p=> dem4a
  dem8p=> dem8a
  t14p => t14a
  t24p => t24a
  t48p => t48a
  t14a = dt(1,4)(0,0.0)
  t24a = dt(2,4)(0,0.0)
  t48a = dt(4,8)(0,0.0)

  t14p = i1p%p14(r4p); print *, t14p%ival, t14p%rval, i1p%ival, r4p%rval
  t24p = i2p%p24(r4p); print *, t24p%ival, t24p%rval, i2p%ival, r4p%rval
  t48p = i4p%p48(r8p); print *, t48p%ival, t48p%rval, i4p%ival, r8p%rval
  print *

  t14p = dit1p%p14(dem4p); print *, t14p%ival, t14p%rval, dit1p%ival, dit1p%ival2, dem4p%rval, dem4p%rval2
  t24p = dit2p%p24(dem4p); print *, t24p%ival, t24p%rval, dit2p%ival, dit2p%ival2, dem4p%rval, dem4p%rval2
  t48p = dit4p%p48(dem8p); print *, t48p%ival, t48p%rval, dit4p%ival, dit4p%ival2, dem8p%rval, dem8p%rval2
  print *

  call display(i1a%p14(r4a), 1)
  call display(i1b%p14(r4b), 1)
  call display(i2a%p24(r4a), 2)
  call display(i2b%p24(r4b), 2)
  call display(i4a%p48(r8a), 4)
  call display(i4b%p48(r8b), 4)
  call display(i1p%p14(r4p), 1)
  call display(i2p%p24(r4p), 2)
  call display(i4p%p48(r8p), 4)
  print *

  call display(dit1a%p14(dem4a), 1)
  call display(dit1b%p14(dem4b), 1)
  call display(dit2a%p24(dem4a), 2)
  call display(dit2b%p24(dem4b), 2)
  call display(dit4a%p48(dem8a), 4)
  call display(dit4b%p48(dem8b), 4)
  call display(dit1p%p14(dem4p), 1)
  call display(dit2p%p24(dem4p), 2)
  call display(dit4p%p48(dem8p), 4)
  print *


  i1a%p14 => fun14a
  i1a%p24 => fun24a
  i1a%p48 => fun48a

  i1b%p14 => fun14
  i1b%p24 => fun24
  i1b%p48 => fun48

  i2a%p14 => fun14a
  i2a%p24 => fun24a
  i2a%p48 => fun48a

  i2b%p14 => fun14
  i2b%p24 => fun24
  i2b%p48 => fun48

  i4a%p14 => fun14a
  i4a%p24 => fun24a
  i4a%p48 => fun48a

  i4b%p14 => fun14
  i4b%p24 => fun24
  i4b%p48 => fun48

  allocate(i1p, source=di(1)(111_1,fun14,fun24a,fun48))
  allocate(i2p, source=di(2)(1111_2,fun14a,fun24a,fun48a))
  allocate(i4p, source=di(4)(111111111_4,fun14a,fun24,fun48a))

  allocate(dit1p, source=ditto(1)(111_1,fun14,fun24a,fun48,99_1))
  allocate(dit2p, source=ditto(2)(1111_2,fun14a,fun24a,fun48a,9999_2))
  allocate(dit4p, source=ditto(4)(111111111_4,fun14a,fun24,fun48a,99999999_4))

  allocate(dem4p, source=demento(4)(1.111111,9.999999_4))
  allocate(dem8p, source=demento(8)(11.111111111111_8,99.999999999_8))

  allocate(t14p, source=dt(1,4)(0,0.0))
  allocate(t24p, source=dt(2,4)(0,0.0))
  allocate(t48p, source=dt(4,8)(0,0.0))

  t14a = i1a%p14(r4a); print *, t14a%ival, t14a%rval, i1a%ival, r4a%rval
  t14b = i1b%p14(r4b); print *, t14b%ival, t14b%rval, i1b%ival, r4b%rval
  t24a = i2a%p24(r4a); print *, t24a%ival, t24a%rval, i2a%ival, r4a%rval
  t24b = i2b%p24(r4b); print *, t24b%ival, t24b%rval, i2b%ival, r4b%rval
  t48a = i4a%p48(r8a); print *, t48a%ival, t48a%rval, i4a%ival, r8a%rval
  t48b = i4b%p48(r8b); print *, t48b%ival, t48b%rval, i4b%ival, r8b%rval
  print *

  t14a = dit1a%p14(dem4a); print *, t14a%ival, t14a%rval, dit1a%ival, dit1a%ival2, dem4a%rval, dem4a%rval2
  t14b = dit1b%p14(dem4b); print *, t14b%ival, t14b%rval, dit1b%ival, dit1b%ival2, dem4b%rval, dem4b%rval2
  t24a = dit2a%p24(dem4a); print *, t24a%ival, t24a%rval, dit2a%ival, dit2a%ival2, dem4a%rval, dem4a%rval2
  t24b = dit2b%p24(dem4b); print *, t24b%ival, t24b%rval, dit2b%ival, dit2b%ival2, dem4b%rval, dem4b%rval2
  t48a = dit4a%p48(dem8a); print *, t48a%ival, t48a%rval, dit4a%ival, dit4a%ival2, dem8a%rval, dem8a%rval2
  t48b = dit4b%p48(dem8b); print *, t48b%ival, t48b%rval, dit4b%ival, dit4b%ival2, dem8b%rval, dem8b%rval2
  print *

  t14p = i1p%p14(r4p); print *, t14p%ival, t14p%rval, i1p%ival, r4p%rval
  t24p = i2p%p24(r4p); print *, t24p%ival, t24p%rval, i2p%ival, r4p%rval
  t48p = i4p%p48(r8p); print *, t48p%ival, t48p%rval, i4p%ival, r8p%rval
  print *

  t14p = dit1p%p14(dem4p); print *, t14p%ival, t14p%rval, dit1p%ival, dit1p%ival2, dem4p%rval, dem4p%rval2
  t24p = dit2p%p24(dem4p); print *, t24p%ival, t24p%rval, dit2p%ival, dit2p%ival2, dem4p%rval, dem4p%rval2
  t48p = dit4p%p48(dem8p); print *, t48p%ival, t48p%rval, dit4p%ival, dit4p%ival2, dem8p%rval, dem8p%rval2
  print *

  call display(i1a%p14(r4a), 1)
  call display(i1b%p14(r4b), 1)
  call display(i2a%p24(r4a), 2)
  call display(i2b%p24(r4b), 2)
  call display(i4a%p48(r8a), 4)
  call display(i4b%p48(r8b), 4)
  call display(i1p%p14(r4p), 1)
  call display(i2p%p24(r4p), 2)
  call display(i4p%p48(r8p), 4)
  print *

  call display(dit1a%p14(dem4a), 1)
  call display(dit1b%p14(dem4b), 1)
  call display(dit2a%p24(dem4a), 2)
  call display(dit2b%p24(dem4b), 2)
  call display(dit4a%p48(dem8a), 4)
  call display(dit4b%p48(dem8b), 4)
  call display(dit1p%p14(dem4p), 1)
  call display(dit2p%p24(dem4p), 2)
  call display(dit4p%p48(dem8p), 4)
  print *

  print *, "done"

end program dtpPPCLocalPassDTPModKFunDTPInh
