!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABasic002
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP variable (length parameter) via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABasic001 ()
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP variable with a length parameter, we verify
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

module dtpIAABasic002mod

  implicit none
  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer      :: ivar(l) = 0
   contains
     final :: fin
  end type dl

contains

  subroutine fin(a)
    type(dl(*)) :: a
    print *, "fin:", a%l, len(a%chvar), size(a%ivar), ">", a%chvar, "<", a%ivar
  end subroutine fin

end module dtpIAABasic002mod



program dtpIAABasic002

  use dtpIAABasic002mod
  implicit none

  type(dl(:)), allocatable :: v1, v2, v3

  print *, allocated(v1), allocated(v2), allocated(v3)
  print *, "v1 = dl(1)('a',[1234321])"
  v1 = dl(1)('a',[1234321])
  print *, "v2 = v1 {v1=", allocated(v1), v1, "}"
  v2 = v1
  v2%chvar = 'b'
  v2%ivar  = [2345432]
  v1%chvar = 'c'
  v1%ivar  = [3456543]
  print *, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"
  v1 = v2
  print *

  print *, "v3 = dl(3)('def',[44444,55555,66666])"
  v3 = dl(3)('def',[44444,55555,66666])
  print *, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  v2 = v3
  print *

  print *, "v3 = v1 {v3=", allocated(v3), v3, ", v1=", allocated(v1), v1, "}"
  v3 = v1
  print *

  print *, allocated(v1), v1%l, len(v1%chvar), size(v1%ivar), v1
  print *, allocated(v2), v2%l, len(v2%chvar), size(v2%ivar), v2
  print *, allocated(v3), v3%l, len(v3%chvar), size(v3%ivar), v3

end program dtpIAABasic002
