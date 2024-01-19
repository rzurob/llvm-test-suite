!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : reference to nopass implicit intrinsic function with intrinsic dummy argument(s), DTP w/ K+L parameters, interface from intrinsic procedure, ref via dummy arg
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCLocalNPassIntrImplIntrinKLFunIntr (<-dtpPPCLocalNPassIntrInterfaceKLSub<-dtpPPCLocalNPassDTPInterfaceKLSub<-dtpPPCBasicNoPassSubroutine)
!*
!*  DESCRIPTION
!*
!*  Create procedure pointers which are references to functions which do not
!*  expect a passed-object dummy argument, but do expect args of intrinsic type
!*  and return an intrinsic type.  Define a parameterised derived type with
!*  procedure pointers and create instances of those types, initialising them
!*  with a structure constructor containing a reference to one of a pair of
!*  routines.  Invoke the referenced procedure via the pointer several times,
!*  and then assign new procedure references and repeat.  Uses dummy args to
!*  reference objects.  The interface is explicit, from the intrinsic procedure
!*  in the pointer declaration.  Almost no intrinsic functions can be used in
!*  this way, however, since they are almost all elemental functions.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyNPassIntrIntrinKLFunIntrmod

  implicit none

  type dt (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: chval
     integer(k)    :: ival
     procedure (len), pointer, nopass :: pI      => null()
  end type dt

contains


  integer(4) function pI4(str)
    character(*), intent(in) :: str
    integer(4), parameter :: ERR4 = z'80000001'
    integer(4), parameter :: END4 = z'80000000'
    read (str,*,err=10,end=11) pI4
    return
10  pI4 = ERR4
    return
11  pI4 = END4
  end function pI4

  integer(4) function pI4a(str)
    character(*), intent(in) :: str
    integer :: i
    pI4a = 0
    do i=1,len(str)
       if (str(i:i)==' ') pI4a = pI4a + 1
    end do
  end function pI4a

end module dtpPPCDummyNPassIntrIntrinKLFunIntrmod


program dtpPPCDummyNPassIntrIntrinKLFunIntr

  use dtpPPCDummyNPassIntrIntrinKLFunIntrmod
  implicit none
  type(dt(4,10)) :: t4a_h
  type(dt(4,2))  :: t4b_h
  type(dt(4,:))  :: t4p_h
  target  :: t4a_h, t4b_h
  pointer :: t4p_h
  integer(4) :: i4result

  t4a_h = dt(4,10)("9999999999",111111111,pI4)
  t4b_h = dt(4,2)("-1",-127,pI4a)
  t4p_h => t4a_h

  call sub(t4a_h, t4b_h, t4p_h)

  t4a_h = dt(4,10)("9999999999",111111111,pI4a)
  t4b_h = dt(4,2)("-1",-127,pI4)
  t4p_h => t4a_h

  call sub(t4a_h, t4b_h, t4p_h)

  print *, "done"

contains

  subroutine sub(t4a, t4b, t4p)
    type(dt(4,10)) :: t4a
    type(dt(4,2))  :: t4b
    type(dt(4,:))  :: t4p

    pointer :: t4p

    print *, pI4("123"), pI4a("123")
    print *, pI4("4 Flowers for Algernon"), pI4a("4 Flowers for Algernon")
    print *, pI4("0 boy"), pI4a("0 boy")

    i4result = t4a % pI("123");                     print *, i4result, t4a % pI("123")
    i4result = t4a % pI("4 Flowers for Algernon");  print *, i4result, t4a % pI("4 Flowers for Algernon")
    i4result = t4a % pI("0 boy");                   print *, i4result, t4a % pI("0 boy")
    i4result = t4b % pI("123");                     print *, i4result, t4b % pI("123")
    i4result = t4b % pI("4 Flowers for Algernon");  print *, i4result, t4b % pI("4 Flowers for Algernon")
    i4result = t4b % pI("0 boy");                   print *, i4result, t4b % pI("0 boy")
    i4result = t4p % pI("123");                     print *, i4result, t4p % pI("123")
    i4result = t4p % pI("4 Flowers for Algernon");  print *, i4result, t4p % pI("4 Flowers for Algernon")
    i4result = t4p % pI("0 boy");                   print *, i4result, t4p % pI("0 boy")

    allocate(t4p, source=dt(4,4)("opqr",123454321,pI4a))
    i4result = t4p % pI("blanks    blanks");        print *, i4result, t4p % pI("blanks    blanks")
  end subroutine sub

end program dtpPPCDummyNPassIntrIntrinKLFunIntr
