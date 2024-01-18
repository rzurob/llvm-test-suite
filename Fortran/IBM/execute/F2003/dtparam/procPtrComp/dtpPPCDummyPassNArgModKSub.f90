!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCDummyPassNArgModKSub
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-03-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : interface spec'd by contained procedures (PASS, subroutines, kind param), ref via dummy arg
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpPPCPassKindSubContained (<-dtpPPCBasicPassKindSub<-dtpPPCBasicPassKindSub<-dtpPPCBasicPass)
!*
!*  DESCRIPTION
!*
!*  Create objects with different KIND params and invoke PASS subroutines on them.
!*  Procedures are specified using a reference to a contained procedure.
!*  References to objects are via dummy arguments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCDummyPassNArgModKSubmod

  implicit none

  type dt (k)
     integer, kind :: k
     integer(k) :: ival
     procedure (pC1), pointer, pass(this) :: p1 => null()
     procedure (pC2), pointer, pass(this) :: p2 => null()
   contains
     procedure, pass :: pC1
     procedure, pass :: pC2
  end type dt

contains

  subroutine pC1(this)
    class(dt(1)), intent(in) :: this
    print *, this % ival, "in pC1"
  end subroutine pC1

  subroutine pC2(this)
    class(dt(2)), intent(in) :: this
    print *, this % ival, "in pC2"
  end subroutine pC2


  subroutine disp1(this)
    class(dt(1)), intent(in) :: this
    print *, this % ival, "in disp1"
  end subroutine disp1

  subroutine disp2(this)
    class(dt(2)), intent(in) :: this
    print *, this % ival, "in disp2"
  end subroutine disp2

  subroutine disp1a(this)
    class(dt(1)), intent(in) :: this
    print *, this % ival, "in disp1a"
  end subroutine disp1a

  subroutine disp2a(this)
    class(dt(2)), intent(in) :: this
    print *, this % ival, "in disp2a"
  end subroutine disp2a

end module dtpPPCDummyPassNArgModKSubmod


program dtpPPCDummyPassNArgModKSub

  use dtpPPCDummyPassNArgModKSubmod
  implicit none
  type(dt(1)) :: t1_h, t1a_h
  type(dt(2)) :: t2_h, t2a_h

  t1_h  = dt(1)(127,disp1,disp2)
  t1a_h = dt(1)(127,disp1a,disp2a)
  t2_h  = dt(2)(32000,disp1,disp2)
  t2a_h = dt(2)(32000,disp1a,disp2a)

  call test

  print *, "done"

contains

  subroutine test

    call t1_h  % pC1
    call t1a_h % pC1
    call t2_h  % pC2
    call t2a_h % pC2

    call runtest(t1_h, t1a_h, t2_h, t2a_h)

    ! Note: we cannot execute "call t1%p2" or "call t2%p1", because there would
    ! be a type mismatch between the invoking variable and the passed-object dummy arg.

    t1_h  % p1 => disp1a
    t1_h  % p2 => disp2a

    t1a_h % p1 => disp1
    t1a_h % p2 => disp2

    t2_h  % p1 => disp1a
    t2_h  % p2 => disp2a

    t2a_h % p1 => disp1
    t2a_h % p2 => disp2

    call runtest(t1_h, t1a_h, t2_h, t2a_h)

  end subroutine test


  subroutine runtest(t1, t1a, t2, t2a)
    type(dt(1)), intent(in) :: t1, t1a
    type(dt(2)), intent(in) :: t2, t2a

    call t1  % p1
    call t1a % p1
    call t2  % p2
    call t2a % p2

  end subroutine runtest


end program dtpPPCDummyPassNArgModKSub
