!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP array (kind) via intrinsic assignment and check lower and upper bounds
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArray001 (<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a kind parameter - we create arrays
!*  with specific bounds, and verify that they are allocated correctly.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABounds001mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar
  end type dk

end module dtpIAABounds001mod



program dtpIAABounds001

  use dtpIAABounds001mod
  implicit none

  type(dk(1)), allocatable :: o1(:), o2(:,:)
  type(dk(1)) :: o1a(3), o1b(-2:1), o2a(2,1), o2b(0:1,-3:-1)
  type(dk(1)) :: o0
  type(dk(4)), allocatable :: v1(:), v3(:,:,:)
  type(dk(4)) :: v1a(2), v1b(1:2), v3a(1,1,1), v3b(2:3,3:4,4:5)
  type(dk(4)) :: v0

  integer :: i

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (allocated(o1) .or. allocated(o2) .or. allocated(v1) .or. allocated(v3)) stop 2

  o1a = [(dk(1)(100+i), i=1,size(o1a))]
  o1b = [(dk(1)(-100+i), i=lbound(o1b,1),ubound(o1b,1))]

  o2a = reshape([dk(1)(49), dk(1)(50)], [2,1])
  o2b = reshape([(dk(1)(10+i), i=1,6)], [2,3])

  o0  = dk(1)(99)

  o1 = o1a
  o2 = o2a

  print *, lbound(o1), ":", ubound(o1), "::", lbound(o1a), ":", ubound(o1a)
  print *, lbound(o2), ":", ubound(o2), "::", lbound(o2a), ":", ubound(o2a)
  if (any(lbound(o1)/=lbound(o1a)) .or. any(ubound(o1)/=ubound(o1a)) &
      .or. any(lbound(o2)/=lbound(o2a)) .or. any(ubound(o2)/=ubound(o2a))) stop 3

  o1 = o1b
  o2 = o2b

  print *, lbound(o1), ":", ubound(o1), "::", lbound(o1b), ":", ubound(o1b)
  print *, lbound(o2), ":", ubound(o2), "::", lbound(o2b), ":", ubound(o2b)
  if (any(lbound(o1)/=lbound(o1b)) .or. any(ubound(o1)/=ubound(o1b)) &
      .or. any(lbound(o2)/=lbound(o2b)) .or. any(ubound(o2)/=ubound(o2b))) stop 4

  o1 = o0
  o2 = o0

  print *, lbound(o1), ":", ubound(o1), "::", lbound(o1b), ":", ubound(o1b)
  print *, lbound(o2), ":", ubound(o2), "::", lbound(o2b), ":", ubound(o2b)
  if (any(lbound(o1)/=lbound(o1b)) .or. any(ubound(o1)/=ubound(o1b)) &
      .or. any(lbound(o2)/=lbound(o2b)) .or. any(ubound(o2)/=ubound(o2b))) stop 5

  o1 = [dk(1)(1), dk(1)(2)]
  o2 = reshape([dk(1)(1), dk(1)(2)], [1,2])

  print *, lbound(o1), ":", ubound(o1), "::", 1, ":", 2
  print *, lbound(o2), ":", ubound(o2), "::", 1,1, ":", 1,2
  if (any(lbound(o1)/=1) .or. any(ubound(o1)/=[2]) &
      .or. any(lbound(o2)/=1) .or. any(ubound(o2)/=[1,2])) stop 6


  v1a = [(dk(4)(100+i), i=1,size(v1a))]
  v1b = [(dk(4)(-100+i), i=lbound(v1b,1),ubound(v1b,1))]

  v3a = dk(4)(49)
  v3b = reshape([(dk(4)(10+i), i=1,8)], [2,2,2])

  v0  = dk(4)(99)

  v1 = v1a
  v3 = v3a

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1a), ":", ubound(v1a)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3a), ":", ubound(v3a)
  if (any(lbound(v1)/=lbound(v1a)) .or. any(ubound(v1)/=ubound(v1a)) &
      .or. any(lbound(v3)/=lbound(v3a)) .or. any(ubound(v3)/=ubound(v3a))) stop 7

  v1 = v1b
  v3 = v3b

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
  if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
      .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) stop 8

  v1 = v0
  v3 = v0

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
  if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
      .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) stop 9

  v1 = [dk(4)(1), dk(4)(2), dk(4)(3)]
  v3 = reshape([(dk(4)(i), i=1,24)], [4,2,3])

  print *, lbound(v1), ":", ubound(v1), "::", 1, ":", 3
  print *, lbound(v3), ":", ubound(v3), "::", 1,1,1, ":", 4,2,3
  if (any(lbound(v1)/=1) .or. any(ubound(v1)/=[3]) &
      .or. any(lbound(v3)/=1) .or. any(ubound(v3)/=[4,2,3])) stop 10

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (.not.(allocated(o1) .and. allocated(o2) .and. allocated(v1) .and. allocated(v3))) stop 10

end program dtpIAABounds001
