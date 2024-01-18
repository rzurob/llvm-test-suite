!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to nopass implicit intrinsic function with intrinsic dummy argument(s), DTP w/ K+L parameters
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalNPassIntrInterfaceKLSub (<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to functions which do not
!*  expect a passed-object dummy argument, only args of intrinsic type.
!*  Define a parameterised derived type with procedure pointers and
!*  create instances of those types, initialising them with a
!*  structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several
!*  times, and then assign new procedure references and repeat.
!*  Uses local variables to reference objects.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCLocalNPassIntrImplIntrinKLFunIntrmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (integer(k)), pointer, nopass   :: pI => null()
     procedure (real(k)), pointer, nopass      :: pR => null()
     procedure (character(4)), pointer, nopass :: pC => null()
  end type dt

contains


  integer(4) function pI4(a1)
    character(*), intent(in) :: a1
    integer(4), parameter :: ERR4 = z'80000001'
    integer(4), parameter :: END4 = z'80000000'
    read (a1,*,err=10,end=11) pI4
    return
10  pI4 = ERR4
    return
11  pI4 = END4
  end function pI4

  integer(8) function pI8(a1)
    character(*), intent(in) :: a1
    integer(8), parameter :: ERR8 = z'8000000000000001'
    integer(8), parameter :: END8 = z'8000000000000000'
    read (a1,*,err=20,end=21) pI8
    return
20  pI8 = ERR8
    return
21  pI8 = END8
  end function pI8


  integer(4) function pI4a(a1)
    integer(4), intent(in) :: a1
    pI4a = -a1
  end function pI4a


  real(4) function pR4(a1,a2,a3)
    integer(4), intent(in) :: a1, a2, a3
    pR4 = a1 * a2 ** real(a3,4)
  end function pR4

  real(8) function pR8(a1,a2,a3)
    integer(8), intent(in) :: a1, a2, a3
    pR8 = (a1 / real(a2,8)) ** a3
  end function pR8


  real(4) function pR4a(a1,a2)
    real(4), intent(in) :: a1, a2
    pR4a = a1 - a2
  end function pR4a


  character(4) function pC4(ch)
    character(1) :: ch
    pC4 = repeat(achar(iachar(ch)-1),4)
  end function pC4

  character(4) function pC4a(ch)
    character(1) :: ch
    pC4a = repeat(achar(iachar(ch)+1),4)
  end function pC4a

end module dtpPPCLocalNPassIntrImplIntrinKLFunIntrmod


program dtpPPCLocalNPassIntrImplIntrinKLFunIntr

  use dtpPPCLocalNPassIntrImplIntrinKLFunIntrmod
  implicit none
  type(dt(4,10)) :: t4a
  type(dt(4,2))  :: t4b
  type(dt(8,4))  :: t8a
  type(dt(4,:))  :: t4p
  type(dt(8,:))  :: t8p
  target  :: t4a, t4b, t8a
  pointer :: t4p, t8p
  integer(4) :: i4result
  integer(8) :: i8result
  real(4)    :: r4result
  real(8)    :: r8result
  character(5) :: chresult

  t4a = dt(4,10)("9999999999",111111111,pI4,pR4,pC4)
  t4b = dt(4,2)("-1",-127,pI4,pR4,pC4)
  t4p => t4a
  t8a = dt(8,4)("abcd",12345678987654321_8,pI8,pR8,pC4a)
  t8p => t8a

  i4result = t4a % pI("123")
  print *, i4result
  i4result = t4a % pI("-124")
  print *, i4result
  i4result = t4a % pI("0")
  print *, i4result
  r4result = t4a % pR(12345,67,2)
  print *, r4result
  chresult = t4a % pC("r")
  print *, ">", chresult, "<"
  print *

  print *, t4b % pI("123")
  print *, t4b % pI("-1234567")
  print *, t4b % pI("")
  print *, t4b % pR(12345,67,2)
  print *, ">", t4b % pC("u"), "<"
  print *

  i4result = t4p % pI("123")
  print *, i4result
  i4result = t4p % pI("-1234567")
  print *, i4result
  i4result = t4p % pI("")
  print *, i4result
  r4result = t4p % pR(12345,67,2)
  print *, r4result
  chresult = t4p % pC("u")
  print *, ">", chresult, "<"
  print *


  i8result = t8a % pI("123")
  print *, i8result
  i8result = t8a % pI("-124")
  print *, i8result
  i8result = t8a % pI("0")
  print *, i8result
  r8result = t8a % pR(11111111212345_8,267_8,2_8)
  print *, r8result
  chresult = t8a % pC("b")
  print *, ">", chresult, "<"
  print *

  print *, t8p % pI("123")
  print *, t8p % pI("-123456789012345")
  print *, t8p % pI("xxx")
  print *, t8p % pR(512345000000_8,167_8,3_8)
  print *, ">", t8p % pC("e"), "<"
  print *


  allocate(t4p, source=dt(4,4)("opqr",123454321,pI4a,pR4a,pC4a))
  ! t4p%pI above called for a single character string arg, but now pI
  ! points to a procedure which expects a single integer.  Similarly,
  ! t4p%pR above called for 3 integer args, but now pR points to a
  ! procedure which takes two reals.  This may be bad style, but
  ! perfectly legal. We'll ignore any warnings the compiler might
  ! generate on the following lines.
  i4result = t4p % pI(95)
  print *, i4result, t4p % pI(95)
  r4result = t4p % pR(3.123,4.345)
  print *, r4result, t4p % pR(3.123,4.345)
  chresult = t4p % pC("m")
  print *, ">", chresult, "/", t4p % pC("m"), "<"
  print *

  print *, "done"

end program dtpPPCLocalNPassIntrImplIntrinKLFunIntr
