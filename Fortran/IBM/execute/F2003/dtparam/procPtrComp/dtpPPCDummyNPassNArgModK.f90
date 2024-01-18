!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : interface spec'd by contained procedures (NOPASS, no args, subroutines & functions, kind param), ref via dummy arg
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCDummyPassNArgModKSub (<-dtpPPCPassKindSubContained<-dtpPPCBasicPassKindSub<-dtpPPCBasicPassKindSub<-dtpPPCBasicPass)
!*
!*  DESCRIPTION
!*
!*  Create objects with different KIND params and invoke NOPASS functions on them.
!*  Procedures are specified using a reference to a module procedure.
!*  Procedures take no arguments.
!*  References to objects are via dummy arguments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyNPassNArgModKmod

  implicit none

  type dt (k)
     integer, kind :: k
     integer(k) :: ival
     procedure (pC1), pointer, nopass :: p1 => null()
     procedure (pC2), pointer, nopass :: p2 => null()
     procedure (pC3), pointer, nopass :: p3 => null()
  end type dt

  character(5) :: stringAns
  integer      :: intAns

contains

  subroutine pC1
    print *, "in pC1"
  end subroutine pC1

  character(5) function pC2()
    pC2 = 'pC2..'
  end function pC2

  integer function pC3()
    pC3 = 123
  end function pC3


  subroutine disp1
    print *, "in disp1"
  end subroutine disp1

  character(5) function str2()
    str2 = stringAns
  end function str2

  integer function int3()
    int3 = intAns
  end function int3


  subroutine disp1a
    print *, "in disp1a"
  end subroutine disp1a

  character(5) function str2a()
    str2a = '#' // stringAns(2:4) // '&'
  end function str2a

  integer function int3a()
    int3a = -intAns
  end function int3a


end module dtpPPCDummyNPassNArgModKmod


program dtpPPCDummyNPassNArgModK

  use dtpPPCDummyNPassNArgModKmod
  implicit none
  type(dt(1)) :: t1_h, t1a_h
  type(dt(2)) :: t2_h, t2a_h

  t1_h  = dt(1)(127,disp1,str2,int3)
  t1a_h = dt(1)(127,disp1a,str2a,int3a)
  t2_h  = dt(2)(32000,disp1,str2,int3)
  t2a_h = dt(2)(32000,disp1a,str2a,int3a)
  stringAns = 'abcde'
  intAns = 91234567

  call test


contains

  subroutine test

    call pC1
    print *, pC2(), pC3()
    print *

    stringAns = 'fghij'
    intAns = 92345678
    call runtest(t1_h, t1a_h, t2_h, t2a_h)

    ! Note: we cannot execute "call t1%p2" or "call t2%p1", because there would
    ! be a type mismatch between the invoking variable and the passed-object dummy arg.

    t1_h  % p1 => disp1a
    t1_h  % p2 => str2a
    t1_h  % p3 => int3a

    t1a_h % p1 => disp1
    t1a_h % p2 => str2
    t1a_h % p3 => int3

    t2_h  % p1 => disp1a
    t2_h  % p2 => str2a
    t2_h  % p3 => int3a

    t2a_h % p1 => disp1
    t2a_h % p2 => str2
    t2a_h % p3 => int3

    stringAns = 'klmno'
    intAns = 93456789

    call runtest(t1_h, t1a_h, t2_h, t2a_h)

    print *, "done"

  end subroutine test


  subroutine runtest(t1, t1a, t2, t2a)
    type(dt(1)), intent(in) :: t1, t1a
    type(dt(2)), intent(in) :: t2, t2a

    call t1 % p1
    call t1a % p1
    stringAns = 'abcde'
    print *, t2 % p2(), t2a % p2(), t2 % p3(), t2a % p3()
    print *

  end subroutine runtest


end program dtpPPCDummyNPassNArgModK
