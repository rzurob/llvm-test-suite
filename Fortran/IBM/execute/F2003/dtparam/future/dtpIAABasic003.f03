!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP variable (container) via intrinsic assignment
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
!*  The simplest case of an allocatable DTP container variable with a kind and a length
!*  parameter, we verify that the variables are initially unallocated, allocated after
!*  assignment, and deallocated before assignment. We can only verify the latter
!*  indirectly, by printing a message when something is finalized, which only happens here
!*  after structure constructors in exprs have been referenced, and after allocated
!*  instances of derived types are deallocated.
!*  There are two important caveats about using finalization in this context:
!*  1. Where multiple objects may be finalized, the precise order of finalization
!*     may be processor dependent.  We cannot avoid this situation below, but we can
!*     ameliorate it by keeping track of where we are in the program (with the variable
!*     "step", which we print with each output), and sorting the outcome after this program
!*     is finished.
!*  2. The entities below are in the main program, and are not finalized immediately
!*     before the END statement.  If the code were executed instead in a subroutine,
!*     additional finalizations would be performed (and the order in which each entity
!*     is finalized would not be guaranteed).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABasic003mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
   contains
     final :: f1, f4
  end type dk

  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer :: iarr(l) = 0
   contains
     final :: fin
  end type dl

  type container(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dk(k)) :: dkvar = dk(k)()
     type(dl(l)) :: dlvar
   contains
     final :: fin1, fin4
  end type container

  integer, save :: step = 0

contains

  subroutine f1(a)
    type(dk(1)) :: a
    print *, step, "f1:", a%ivar
    if( a%k /= 1 .or. a%k /= kind(a%ivar)) error stop 2
  end subroutine f1

  subroutine f4(a)
    type(dk(4)) :: a
    print *, step, "f4:", a%ivar
    if( a%k /= 4 .or. a%k /= kind(a%ivar)) error stop 3
  end subroutine f4

  subroutine fin(a)
    type(dl(*)) :: a
    print *, step, "fin:", a%l, ">", a%chvar, "<", a%iarr
    if( a%l /= len(a%chvar) .or. a%l /= size(a%iarr)) error stop 4
  end subroutine fin

  subroutine fin1(a)
    type(container(1,*)) :: a
    print *, step, "fin1:", a%l, a%dkvar, a%dlvar
    if (a%k /= 1 .or. a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) error stop 5
    if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l .or. size(a%dlvar%iarr) /= a%l) error stop 6
  end subroutine fin1

  subroutine fin4(a)
    type(container(4,*)) :: a
    print *, step, "fin4:", a%l, a%dkvar, a%dlvar
    if (a%k /= 4 .or. a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) error stop 7
    if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l .or. size(a%dlvar%iarr) /= a%l) error stop 8
  end subroutine fin4

end module dtpIAABasic003mod



program dtpIAABasic003

  use dtpIAABasic003mod
  implicit none

  type(container(1,:)), allocatable :: o1, o2, o3
  type(container(4,:)), allocatable :: v1, v2, v3

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
  ! start at step 10 so that the later sort doesn't put "12 ..." before "3 ...":
  step = 10
  print *, step, allocated(o1), allocated(o2), allocated(o3)

  step = 11
  print *, step, "o1 = container(1,2)(dk(1)(34),dl(2)('ab',[35,36]))"

  step = 12
  o1 = container(1,2)(dk(1)(34),dl(2)('ab',[35,36]))

  step = 13
  print *, step, "o2 = container(1,2)(dk(1)(37),dl(2)('cd',[38,39]))"

  step = 14
  o2 = container(1,2)(dk(1)(37),dl(2)('cd',[38,39]))

  step = 15
  print *, step, "o1 = o2 {o1=", allocated(o1), o1, ", o2=", allocated(o2), o2, "}"

  step = 16
  o1 = o2

  step = 17
  print *, step, "o3 = container(1,3)(dk(1)(40),dl(3)('efg',[41,42,43]))"
  step = 18
  o3 = container(1,3)(dk(1)(40),dl(3)('efg',[41,42,43]))

  step = 19
  print *, step, "o2 = o3 {o2=", allocated(o2), o2, ", o3=", allocated(o3), o3, "}"
  step = 20
  o2 = o3

  ! Repeat the above with a container of kind 4
  step = 21
  print *, step, allocated(v1), allocated(v2), allocated(v3)

  step = 22
  print *, step, "v1 = container(4,2)(dk(4)(44),dl(2)('AB',[45,46]))"

  step = 23
  v1 = container(4,2)(dk(4)(44),dl(2)('AB',[45,46]))

  step = 24
  print *, step, "v2 = container(4,2)(dk(4)(47),dl(2)('CD',[48,49]))"

  step = 25
  v2 = container(4,2)(dk(4)(47),dl(2)('CD',[48,49]))

  step = 26
  print *, step, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"

  step = 27
  v1 = v2

  step = 28
  print *, step, "v3 = container(4,3)(dk(4)(50),dl(3)('EFG',[51,52,53]))"
  step = 29
  v3 = container(4,3)(dk(4)(50),dl(3)('EFG',[51,52,53]))

  step = 30
  print *, step, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  step = 31
  v2 = v3

  ! Just use a higher number in place of "step":
  print *, 91, allocated(o1), o1%k, o1%l, len(o1%dlvar%chvar), size(o1%dlvar%iarr), o1
  print *, 92, allocated(o2), o2%k, o2%l, len(o2%dlvar%chvar), size(o2%dlvar%iarr), o2
  print *, 93, allocated(o3), o3%k, o3%l, len(o3%dlvar%chvar), size(o3%dlvar%iarr), o3

  print *, 94, allocated(v1), v1%k, v1%l, len(v1%dlvar%chvar), size(v1%dlvar%iarr), v1
  print *, 95, allocated(v2), v2%k, v2%l, len(v2%dlvar%chvar), size(v2%dlvar%iarr), v2
  print *, 96, allocated(v3), v3%k, v3%l, len(v3%dlvar%chvar), size(v3%dlvar%iarr), v3

end program dtpIAABasic003
