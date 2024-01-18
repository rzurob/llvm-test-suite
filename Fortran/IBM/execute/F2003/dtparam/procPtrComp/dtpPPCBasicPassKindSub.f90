!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPPCBasicPassKindSub
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-03-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : pointer to PASS subroutines from DTP with kind param
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpPPCBasicPass ()
!*
!*  DESCRIPTION
!*
!*  Create objects with different KIND params and invoke PASS subroutines on them.
!*  Procedures are specified using an interface.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCBasicPassKindSubmod

  implicit none

  type dt (k)
     integer, kind :: k
     integer(k) :: ival
     procedure (s1), pointer, pass :: p1 => null()
     procedure (s2), pointer, pass :: p2 => null()
  end type dt

  abstract interface

     subroutine s1(this)
       import :: dt
       class(dt(1)), intent(in) :: this
     end subroutine s1

     subroutine s2(this)
       import :: dt
       class(dt(2)), intent(in) :: this
     end subroutine s2

  end interface


contains


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

end module dtpPPCBasicPassKindSubmod


program dtpPPCBasicPassKindSub

  use dtpPPCBasicPassKindSubmod
  implicit none
  type(dt(1)) :: t1, t1a
  type(dt(2)) :: t2, t2a

  t1  = dt(1)(127,disp1,disp2)
  t1a = dt(1)(127,disp1a,disp2a)
  t2  = dt(2)(32000,disp1,disp2)
  t2a = dt(2)(32000,disp1a,disp2a)

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

end program dtpPPCBasicPassKindSub
