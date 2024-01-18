!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (len) via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABasic002 (<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a length parameter, we verify
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

module dtpIAAArray002mod

  implicit none
  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer      :: ivar(l) = 0
   contains
     final :: fin, finArray
  end type dl

  integer, save :: step = 0

contains

  subroutine fin(a)
    type(dl(*)) :: a
    print *, step, "fin:", a%l, len(a%chvar), size(a%ivar), ">", a%chvar, "<", a%ivar
  end subroutine fin

  subroutine finArray(a)
    type(dl(*)) :: a(:)
    integer :: i
    print *, step, "finArray:", a%l, size(a), len(a%chvar), ">", a%chvar, "<", (a(i)%ivar,i=1,size(a))
  end subroutine finArray

end module dtpIAAArray002mod



program dtpIAAArray002

  use dtpIAAArray002mod
  implicit none

  type(dl(:)), allocatable :: v1(:), v2(:), v3(:)
  type(dl(4)) :: v4

  step = 10
  print *, step, allocated(v1), allocated(v2), allocated(v3)

  step = 11
  print *, step, "v1 = [dl(1)('a',[1234321])]"
  step = 12
  v1 = [dl(1)('a',[1234321])]

  step = 13
  print *, step, "v2 = v1 {v1=", allocated(v1), v1, "}"
  step = 14
  v2 = v1

  step = 15
  print *, step, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"
  step = 16
  v1 = v2

  step = 17
  print *, step, "v3 = [dl(3)('def',[44444,55555,66666]), dl(3)('ghi',[77777,88888,99999])]"
  step = 18
  v3 = [dl(3)('def',[44444,55555,66666]), dl(3)('ghi',[77777,88888,99999])]

  step = 19
  print *, step, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  step = 20
  v2 = v3

  step = 21
  print *, step, "v3 = v1 {v3=", allocated(v3), v3, ", v1=", allocated(v1), v1, "}"
  step = 22
  v3 = v1

  step = 23
  print *, step, "v4 = dl(4)('jklm',[4040404,3030303,2020202,1010101])"
  step = 24
  v4 = dl(4)('jklm',[4040404,3030303,2020202,1010101])

  step = 25
  print *, step, "v1 = v4 {v1=", allocated(v1), v1, ", v4=", v4, "}"
  step = 26
  v1 = v4

  print *, 91, allocated(v1), v1%l, len(v1%chvar), v1
  print *, 92, allocated(v2), v2%l, len(v2%chvar), v2
  print *, 93, allocated(v3), v3%l, len(v3%chvar), v3

end program dtpIAAArray002
