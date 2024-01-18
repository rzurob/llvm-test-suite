!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCPassKindSubContained
!*
!*  DATE                       : 2009-03-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : interface spec'd by contained procedures (PASS, subroutines, kind param)
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCBasicPassKindSub (<-dtpPPCBasicPassKindSub<-dtpPPCBasicPass)
!*
!*  DESCRIPTION
!*
!*  Create objects with different KIND params and invoke PASS functions on them.
!*  Procedures are specified using a reference to a contained procedure.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCPassKindSubContainedmod

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

end module dtpPPCPassKindSubContainedmod


program dtpPPCPassKindSubContained

  use dtpPPCPassKindSubContainedmod
  implicit none
  type(dt(1)) :: t1, t1a
  type(dt(2)) :: t2, t2a

  t1  = dt(1)(127,disp1,disp2)
  t1a = dt(1)(127,disp1a,disp2a)
  t2  = dt(2)(32000,disp1,disp2)
  t2a = dt(2)(32000,disp1a,disp2a)

  call t1  % pC1
  call t1a % pC1
  call t2  % pC2
  call t2a % pC2

  call t1  % p1
  call t1a % p1
  call t2  % p2
  call t2a % p2

  ! Note: we cannot execute "call t1%p2" or "call t2%p1", because there would
  ! be a type mismatch between the invoking variable and the passed-object dummy arg.

  t1  % p1 => disp1a
  t1  % p2 => disp2a

  t1a % p1 => disp1
  t1a % p2 => disp2

  t2  % p1 => disp1a
  t2  % p2 => disp2a

  t2a % p1 => disp1
  t2a % p2 => disp2

  call t1  % p1
  call t1a % p1
  call t2  % p2
  call t2a % p2

  print *, "done"

end program dtpPPCPassKindSubContained
