!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAAArray001
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (kind) via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAABasic001 ()
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a kind parameter, we verify
!*  that the variables are initially unallocated, allocated after assignment, and deallocated
!*  before assignment. We can only verify the latter indirectly, by printing a message when
!*  something is finalized, which only happens here after structure constructors in exprs
!*  have been referenced, and after allocated instances of derived types are deallocated.
!*  There are two important caveats about using finalization in this context:
!*  1. Where multiple objects may be finalized, the precise order of finalization
!*     may be processor dependent.  The program below has been constructed so as to
!*     avoid this situation.
!*  2. The entities below are in the main program, and are not finalized immediately
!*     before the END statement.  If the code were executed instead in a subroutine,
!*     additional finalizations would be performed (and the order in which each entity
!*     is finalized would not be guaranteed).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArray001mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
   contains
     final :: f1, f1arr, f4, f4arr
  end type dk

  integer, save :: step = 0

contains

  subroutine f1(a)
    type(dk(1)) :: a
    print *, step, "f1:", a%k, kind(a%ivar), a%ivar
  end subroutine f1

  subroutine f1arr(a)
    type(dk(1)) :: a(:)
    print *, step, "f1arr:", a%k, kind(a%ivar), size(a), a%ivar
  end subroutine f1arr

  subroutine f4(a)
    type(dk(4)) :: a
    print *, step, "f4:", a%k, kind(a%ivar), a%ivar
  end subroutine f4

  subroutine f4arr(a)
    type(dk(4)) :: a(:)
    print *, step, "f4arr:", a%k, kind(a%ivar), size(a), a%ivar
  end subroutine f4arr

end module dtpIAAArray001mod



program dtpIAAArray001

  use dtpIAAArray001mod
  implicit none

  type(dk(1)), allocatable :: v1(:), v1a(:)
  type(dk(1)) :: v1b
  type(dk(4)), allocatable :: v4(:), v4a(:)
  type(dk(4)) :: v4b

  step = 10
  print *, step, allocated(v1), allocated(v1a)

  step = 11
  print *, step, "v1 = [dk(1)(101)]"
  step = 12
  v1 = [dk(1)(101)]

  step = 13
  print *, step, "v1a = v1 {v1=", allocated(v1), v1, "}"
  step = 14
  v1a = v1

  step = 15
  print *, step, "v1 = v1a {v1=", allocated(v1), v1, ", v1a=", allocated(v1a), v1a, "}"
  step = 16
  v1 = v1a

  step = 17
  print *, step, "v1b = dk(1)(102)"
  step = 18
  v1b = dk(1)(102)

  step = 19
  print *, step, "v1a = v1b {v1a=", allocated(v1a), v1a, ", v1b=", v1b, "}"
  step = 20
  v1a = v1b

  step = 21
  print *, step, allocated(v4), allocated(v4a)

  step = 22
  print *, step, "v4 = [dk(4)(20000002), dk(4)(30000003), dk(4)(40000004)]"
  step = 23
  v4 = [dk(4)(20000002), dk(4)(30000003), dk(4)(40000004)]

  step = 24
  print *, step, "v4a = v4 {v4=", allocated(v4), v4, "}"
  step = 25
  v4a = v4

  step = 26
  print *, step, "v4 = v4a {v4=", allocated(v4), v4, ", v4a=", allocated(v4a), v4a, "}"
  step = 27
  v4 = v4a

  step = 28
  print *, step, "v4b = dk(4)(50000005)"
  step = 29
  v4b = dk(4)(50000005)

  step = 30
  print *, step, "v4a = v4b {v4a=", allocated(v4a), v4a, ", v4b=", v4b, "}"
  step = 31
  v4a = v4b

  print *, 91, allocated(v1),  v1%k,  kind(v1%ivar),  v1
  print *, 92, allocated(v1a), v1a%k, kind(v1a%ivar), v1a
  print *, 92, allocated(v4),  v4%k,  kind(v4%ivar),  v4
  print *, 93, allocated(v4a), v4a%k, kind(v4a%ivar), v4a

end program dtpIAAArray001
