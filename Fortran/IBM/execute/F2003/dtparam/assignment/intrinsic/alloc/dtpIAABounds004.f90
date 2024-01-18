!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (kind parameter) with allocatable component via intrinsic assignment and check lower and upper bounds
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArray004 (<-dtpIAABasic004)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a kind parameter, with allocatable
!*  components - we create arrays with specific bounds, and verify that they are allocated correctly.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABounds004mod

  implicit none
  type dka(k)
     integer, kind :: k
     integer(k), allocatable :: ivar
  end type dka

end module dtpIAABounds004mod



program dtpIAABounds004

  use dtpIAABounds004mod
  implicit none

  type(dka(1)), allocatable :: o1(:), o2(:,:)
  type(dka(1)) :: o1a(3), o1b(-2:1), o2a(2,1), o2b(0:1,-3:-1)
  type(dka(1)) :: o0
  type(dka(4)), allocatable :: v1(:), v3(:,:,:)
  type(dka(4)) :: v1a(2), v1b(1:2), v3a(1,1,1), v3b(2:3,3:4,4:5)
  type(dka(4)) :: v0

  integer :: i

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (allocated(o1) .or. allocated(o2) .or. allocated(v1) .or. allocated(v3)) error stop 2

  o1a = [(dka(1)(100+i), i=1,size(o1a))]
  o1b = [(dka(1)(-100+i), i=lbound(o1b,1),ubound(o1b,1))]

  o2a = reshape([dka(1)(49), dka(1)(50)], [2,1])
  o2b = reshape([(dka(1)(10+i), i=1,6)], [2,3])

  o0  = dka(1)(99)

  o1 = o1a
  o2 = o2a

  print *, lbound(o1), ":", ubound(o1), "::", lbound(o1a), ":", ubound(o1a)
  print *, lbound(o2), ":", ubound(o2), "::", lbound(o2a), ":", ubound(o2a)
  if (any(lbound(o1)/=lbound(o1a)) .or. any(ubound(o1)/=ubound(o1a)) &
      .or. any(lbound(o2)/=lbound(o2a)) .or. any(ubound(o2)/=ubound(o2a))) error stop 3

  o1 = o1b
  o2 = o2b

  print *, lbound(o1), ":", ubound(o1), "::", lbound(o1b), ":", ubound(o1b)
  print *, lbound(o2), ":", ubound(o2), "::", lbound(o2b), ":", ubound(o2b)
  if (any(lbound(o1)/=lbound(o1b)) .or. any(ubound(o1)/=ubound(o1b)) &
      .or. any(lbound(o2)/=lbound(o2b)) .or. any(ubound(o2)/=ubound(o2b))) error stop 4

  o1 = o0
  o2 = o0

  print *, lbound(o1), ":", ubound(o1), "::", lbound(o1b), ":", ubound(o1b)
  print *, lbound(o2), ":", ubound(o2), "::", lbound(o2b), ":", ubound(o2b)
  if (any(lbound(o1)/=lbound(o1b)) .or. any(ubound(o1)/=ubound(o1b)) &
      .or. any(lbound(o2)/=lbound(o2b)) .or. any(ubound(o2)/=ubound(o2b))) error stop 5

  o1 = [dka(1)(1), dka(1)(2)]
  o2 = reshape([dka(1)(1), dka(1)(2)], [1,2])

  print *, lbound(o1), ":", ubound(o1), "::", 1, ":", 2
  print *, lbound(o2), ":", ubound(o2), "::", 1,1, ":", 1,2
  if (any(lbound(o1)/=1) .or. any(ubound(o1)/=[2]) &
      .or. any(lbound(o2)/=1) .or. any(ubound(o2)/=[1,2])) error stop 6


  v1a = [(dka(4)(100+i), i=1,size(v1a))]
  v1b = [(dka(4)(-100+i), i=lbound(v1b,1),ubound(v1b,1))]

  v3a = dka(4)(49)
  v3b = reshape([(dka(4)(10+i), i=1,8)], [2,2,2])

  v0  = dka(4)(99)

  v1 = v1a
  v3 = v3a

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1a), ":", ubound(v1a)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3a), ":", ubound(v3a)
  if (any(lbound(v1)/=lbound(v1a)) .or. any(ubound(v1)/=ubound(v1a)) &
      .or. any(lbound(v3)/=lbound(v3a)) .or. any(ubound(v3)/=ubound(v3a))) error stop 7

  v1 = v1b
  v3 = v3b

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
  if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
      .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) error stop 8

  v1 = v0
  v3 = v0

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
  if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
      .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) error stop 9

  v1 = [dka(4)(1), dka(4)(2), dka(4)(3)]
  v3 = reshape([(dka(4)(i), i=1,24)], [4,2,3])

  print *, lbound(v1), ":", ubound(v1), "::", 1, ":", 3
  print *, lbound(v3), ":", ubound(v3), "::", 1,1,1, ":", 4,2,3
  if (any(lbound(v1)/=1) .or. any(ubound(v1)/=[3]) &
      .or. any(lbound(v3)/=1) .or. any(ubound(v3)/=[4,2,3])) error stop 10

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (.not.(allocated(o1) .and. allocated(o2) .and. allocated(v1) .and. allocated(v3))) error stop 10

end program dtpIAABounds004
