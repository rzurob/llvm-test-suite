!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCModPassDTPInterfaceKFunDTP
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-03-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to pass function, DTP arg + result w/ K parameter, ref via module var
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpPPCModNPassDTPInterfaceKLFunDTP (<-dtpPPCLocalNPassDTPInterfaceKLFunDTP<-dtpPPCLocalNPassDTPInterfaceKLFunIntr<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
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
!*  Uses module variables to reference objects.
!*  The type has a simple kind parameter.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCModPassDTPInterfaceKFunDTPmod

  implicit none

  type di (k)
     integer, kind :: k
     integer(k)   :: ival
     procedure (f14), pointer, pass :: p14 => null()
     procedure (f24), pointer, pass :: p24 => null()
     procedure (f48), pointer, pass :: p48 => null()
  end type di

  type dr (k)
     integer, kind :: k
     real(k)       :: rval
  end type dr

  type dt (k1,k2)
     integer, kind :: k1, k2
     integer(k1)   :: ival
     real(k2)      :: rval
  end type dt


  abstract interface

     type(dt(1,4)) function f14(a1,a2)
       import :: di, dr, dt
       class(di(1)), intent(in) :: a1
       class(dr(4)), intent(in) :: a2
     end function f14

     type(dt(2,4)) function f24(a1,a2)
       import :: di, dr, dt
       class(di(2)), intent(in) :: a1
       class(dr(4)), intent(in) :: a2
     end function f24

     type(dt(4,8)) function f48(a1,a2)
       import :: di, dr, dt
       class(di(4)), intent(in) :: a1
       class(dr(8)), intent(in) :: a2
     end function f48

  end interface


  type(di(1)) :: i1a, i1b, i1p
  type(di(2)) :: i2a, i2b, i2p
  type(di(4)) :: i4a, i4b, i4p

  type(dr(4)) :: r4a, r4b, r4p
  type(dr(8)) :: r8a, r8b, r8p

  type(dt(1,4)) :: t14a, t14b, t14p
  type(dt(2,4)) :: t24a, t24b, t24p
  type(dt(4,8)) :: t48a, t48b, t48p

  target  :: i1a, i2a, i4a, r4a, r8a, t14a, t24a, t48a
  pointer :: i1p, i2p, i4p, r4p, r8p, t14p, t24p, t48p

  save :: i1a, i1b, i2a, i2b, i4a, i4b

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
    fun14a%ival = -this%ival
    fun14a%rval = 1/that%rval
  end function fun14a

  type(dt(2,4)) function fun24a(this, that)
    class(di(2)), intent(in) :: this
    class(dr(4)), intent(in) :: that
    fun24a%ival = -this%ival
    fun24a%rval = 1/that%rval
  end function fun24a


  type(dt(4,8)) function fun48a(this, that)
    class(di(4)), intent(in) :: this
    class(dr(8)), intent(in) :: that
    fun48a%ival = -this%ival
    fun48a%rval = 1/that%rval
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

end module dtpPPCModPassDTPInterfaceKFunDTPmod


program dtpPPCModPassDTPInterfaceKFunDTP

  use dtpPPCModPassDTPInterfaceKFunDTPmod
  implicit none

  i1a = di(1)(127, fun14,fun24,fun48)
  i1b = di(1)(127, fun14a,fun24a,fun48a)
  i2a = di(2)(32000, fun14,fun24,fun48)
  i2b = di(2)(-1234, fun14a,fun24a,fun48a)
  i4a = di(4)(200032000, fun14,fun24,fun48)
  i4b = di(4)(-12341234, fun14a,fun24a,fun48a)

  r4a = dr(4)(9.876543210987654321)
  r4b = dr(4)(1.234567890123456789)
  r8a = dr(8)(9.876543210987654321_8)
  r8b = dr(8)(1.234567890123456789_8)

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

  i1p  => i1a
  i2p  => i2a
  i4p  => i4a
  r4p  => r4a
  r8p  => r8a
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

  t14p = i1p%p14(r4p); print *, t14p%ival, t14p%rval, i1p%ival, r4p%rval
  t24p = i2p%p24(r4p); print *, t24p%ival, t24p%rval, i2p%ival, r4p%rval
  t48p = i4p%p48(r8p); print *, t48p%ival, t48p%rval, i4p%ival, r8p%rval
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

  print *, "done"

end program dtpPPCModPassDTPInterfaceKFunDTP
