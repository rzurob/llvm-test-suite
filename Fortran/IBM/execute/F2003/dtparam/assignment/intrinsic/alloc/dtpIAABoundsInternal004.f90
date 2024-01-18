!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABoundsInternal004
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (kind parameter) with allocatable component via intrinsic assignment and check lower and upper bounds (in internal subroutine)
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAABounds004 (<-dtpIAAArray004<-dtpIAABasic004)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a kind parameter, with allocatable
!*  components - we create arrays with specific bounds, and verify that they are allocated correctly.
!*  This adaptation passes variables and literals in to several test subroutines,
!*  some of which set values, and others of which test the results.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABoundsInternal004mod

  implicit none
  type dka(k)
     integer, kind :: k
     integer(k), allocatable :: ivar
  end type dka

end module dtpIAABoundsInternal004mod



program dtpIAABoundsInternal004

  use dtpIAABoundsInternal004mod
  implicit none

  type(dka(1)), allocatable :: o1(:), o2(:,:)
  type(dka(1)) :: o1a(3), o1b(-2:1), o2a(2,1), o2b(0:1,-3:-1)
  type(dka(1)) :: o0
  type(dka(4)), allocatable :: v1(:), v3(:,:,:)
  type(dka(4)) :: v1a(2), v1b(1:2), v3a(1,1,1), v3b(2:3,3:4,4:5)
  type(dka(4)) :: v0

  integer :: i

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (allocated(o1) .or. allocated(o2) .or. allocated(v1) .or. allocated(v3)) stop 2

  call osetup(o1a,o1b,o2a,o2b,o0)

  call oAssign1(o1,o1a)
  call oCheck1(o1,o1a)

  call oAssign2(o2,o2a)
  call oCheck2(o2,o2a)

  call oAssign1(o1,o1b)
  call oCheck1(o1,o1b)

  call oAssign2(o2,o2b)
  call oCheck2(o2,o2b)

  call oAssign0(o1,o2,o0)
  call oCheck1(o1,o1b)
  call oCheck2(o2,o2b)

  call oAssign1(o1,[dka(1)(1), dka(1)(2)])
  call oCheck1(o1,[dka(1)(1), dka(1)(2)])

  call oAssign2(o2,reshape([dka(1)(1), dka(1)(2)], [1,2]))
  call oCheck2(o2,reshape([dka(1)(1), dka(1)(2)], [1,2]))

  call vsetup(v1a,v1b,v3a,v3b,v0)

  call vAssign1(v1,v1a)
  call vCheck1(v1,v1a)

  call vAssign2(v3,v3a)
  call vCheck2(v3,v3a)

  call vAssign1(v1,v1b)
  call vCheck1(v1,v1b)

  call vAssign2(v3,v3b)
  call vCheck2(v3,v3b)

  call vAssign0(v1,v3,v0)
  call vCheck1(v1,v1b)
  call vCheck2(v3,v3b)

  call vAssign1(v1,[dka(4)(1), dka(4)(2), dka(4)(3)])
  call vCheck1(v1,[dka(4)(1), dka(4)(2), dka(4)(3)])

  call vAssign2(v3,reshape([(dka(4)(i), i=1,24)], [4,2,3]))
  call vCheck2(v3,reshape([(dka(4)(i), i=1,24)], [4,2,3]))

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (.not.(allocated(o1) .and. allocated(o2) .and. allocated(v1) .and. allocated(v3))) stop 7

contains

  subroutine osetup(o1a,o1b,o2a,o2b,o0)
    type(dka(1)) :: o1a(:), o1b(:), o2a(:,:), o2b(:,:)
    type(dka(1)) :: o0

    o1a = [(dka(1)(100+i), i=1,size(o1a))]
    o1b = [(dka(1)(-100+i), i=lbound(o1b,1),ubound(o1b,1))]

    o2a = reshape([dka(1)(49), dka(1)(50)], [2,1])
    o2b = reshape([(dka(1)(10+i), i=1,6)], [2,3])

    o0  = dka(1)(99)
  end subroutine osetup

  subroutine oAssign1(o1,o1a)
    type(dka(1)), allocatable :: o1(:)
    type(dka(1)) :: o1a(:)
    o1 = o1a
  end subroutine oAssign1

  subroutine oCheck1(o1,o1a)
    type(dka(1)), allocatable :: o1(:)
    type(dka(1)) :: o1a(:)

    print *, lbound(o1), ":", ubound(o1), "::", lbound(o1a), ":", ubound(o1a)
    if (any(lbound(o1)/=lbound(o1a)) .or. any(ubound(o1)/=ubound(o1a))) stop 3
  end subroutine oCheck1

  subroutine oAssign2(o2,o2a)
    type(dka(1)), allocatable :: o2(:,:)
    type(dka(1)) :: o2a(:,:)
    o2 = o2a
  end subroutine oAssign2

  subroutine oCheck2(o2,o2a)
    type(dka(1)), allocatable :: o2(:,:)
    type(dka(1)) :: o2a(:,:)

    print *, lbound(o2), ":", ubound(o2), "::", lbound(o2a), ":", ubound(o2a)
    if (any(lbound(o2)/=lbound(o2a)) .or. any(ubound(o2)/=ubound(o2a))) stop 4
  end subroutine oCheck2

  subroutine oAssign0(o1,o2,o0)
    type(dka(1)), allocatable :: o1(:), o2(:,:)
    type(dka(1)) :: o0
    o1 = o0
    o2 = o0
  end subroutine oAssign0

  subroutine vsetup(v1a,v1b,v3a,v3b,v0)
    type(dka(4)) :: v1a(:), v1b(:), v3a(:,:,:), v3b(:,:,:)
    type(dka(4)) :: v0

    v1a = [(dka(4)(100+i), i=1,size(v1a))]
    v1b = [(dka(4)(-100+i), i=lbound(v1b,1),ubound(v1b,1))]

    v3a = dka(4)(49) ! this is a 1x1x1 array ...
    v3b = reshape([(dka(4)(10+i), i=1,8)], [2,2,2])

    v0  = dka(4)(99)
  end subroutine vsetup

  subroutine vAssign1(v1,v1a)
    type(dka(4)), allocatable :: v1(:)
    type(dka(4)) :: v1a(:)
    v1 = v1a
  end subroutine vAssign1

  subroutine vCheck1(v1,v1a)
    type(dka(4)), allocatable :: v1(:)
    type(dka(4)) :: v1a(:)

    print *, lbound(v1), ":", ubound(v1), "::", lbound(v1a), ":", ubound(v1a)
    if (any(lbound(v1)/=lbound(v1a)) .or. any(ubound(v1)/=ubound(v1a))) stop 5
  end subroutine vCheck1

  subroutine vAssign2(v3,v3a)
    type(dka(4)), allocatable :: v3(:,:,:)
    type(dka(4)) :: v3a(:,:,:)
    v3 = v3a
  end subroutine vAssign2

  subroutine vCheck2(v3,v3a)
    type(dka(4)), allocatable :: v3(:,:,:)
    type(dka(4)) :: v3a(:,:,:)

    print *, lbound(v3), ":", ubound(v3), "::", lbound(v3a), ":", ubound(v3a)
    if (any(lbound(v3)/=lbound(v3a)) .or. any(ubound(v3)/=ubound(v3a))) stop 6
  end subroutine vCheck2

  subroutine vAssign0(v1,v3,v0)
    type(dka(4)), allocatable :: v1(:), v3(:,:,:)
    type(dka(4)) :: v0
    v1 = v0
    v3 = v0
  end subroutine vAssign0

end program dtpIAABoundsInternal004
