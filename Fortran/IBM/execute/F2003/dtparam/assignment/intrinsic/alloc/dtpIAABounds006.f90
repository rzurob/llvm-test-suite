!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABounds006
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (container) with indirectly allocatable components via intrinsic assignment and check lower and upper bounds
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArray006 (<-dtpIAABasic006<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP container array with a kind and a length
!*  parameter, but with references to types which themselves have allocatable components,
!*  - we create arrays with specific bounds, and verify that they are allocated correctly.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABounds006mod

  implicit none
  type dka(k)
     integer, kind :: k
     integer(k), allocatable :: ivar
  end type dka

  type dla(l)
     integer, len :: l
     character(l) :: label = ''
     integer, allocatable :: iarr(:)
  end type dla

  type acontainer(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dka(k)) :: dkvar
     type(dla(l)) :: dlvar
  end type acontainer
end module dtpIAABounds006mod



program dtpIAABounds006

  use dtpIAABounds006mod
  implicit none

  type(acontainer(1,:)), allocatable :: o1(:), o2(:,:)
  type(acontainer(1,2)) :: o11(2), o22(2)
  type(acontainer(1,2)) :: o1a(3), o1b(-4:-1), o2a(1,2), o2b(-1003:-1001,10:11)
  type(acontainer(1,2)) :: o1aa(3), o1bb(-4:-1), o2aa(2), o2bb(6)
  type(acontainer(1,4)) :: o0
  type(acontainer(4,:)), allocatable :: v1(:), v3(:,:,:)
  type(acontainer(4,5)) :: v11(3), v33(24)
  type(acontainer(4,5)) :: v1a(2), v1b(1:2), v3a(1,1,1), v3b(4:5,3:4,2:3)
  type(acontainer(4,5)) :: v1aa(2), v1bb(1:2), v3bb(8)
  type(acontainer(4,4)) :: v0

  integer :: i, j, five(5)

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (allocated(o1) .or. allocated(o2) .or. allocated(v1) .or. allocated(v3)) stop 2

  five = [1,3,5,7,9]
  do i = 1, 3
    o1aa(i) = acontainer(1,2)(dka(1)(100+i), dla(2)(repeat(achar(99+i),2),[50+i,500+i]))
  end do
  o1a = o1aa

  do i = lbound(o1b,1),ubound(o1b,1)
    o1bb(i) = acontainer(1,2)(dka(1)(10+i), dla(2)(repeat(achar(69+i),2),[550+i,5500+i]))
  end do
  o1b = o1bb

  o2aa(1) = acontainer(1,2)(dka(1)(49), dla(2)('xy',[1,2]))
  o2aa(2) = acontainer(1,2)(dka(1)(50), dla(2)('za',[2,1]))
  o2a = reshape(o2aa, [1,2])

  do i = 1, 6
    o2bb(i) = acontainer(1,2)(dka(1)(10+i), dla(2)('xx',[10+i,11+i]))
  end do

  o2b = reshape (o2bb, [3,2])

  o0  = acontainer(1,4)(dka(1)(99), dla(4)('uvwx',[42,24,-42,-24]))

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

  o11(1) = acontainer(1,2)(dka(1)(1),dla(2)('jj',[12,13]))
  o11(2) = acontainer(1,2)(dka(1)(1),dla(2)('jj',[12,13]))
  o1 = o11

  o22(1) = acontainer(1,2)(dka(1)(1), dla(2)('qq',[-1,-2]))
  o22(2) = acontainer(1,2)(dka(1)(1), dla(2)('qq',[-1,-2]))
  o2 = reshape(o22, [1,2])

!  o1 = [acontainer(1,2)(dka(1)(1),dla(2)('jj',[12,13])), acontainer(1,2)(dka(1)(1),dla(2)('jj',[12,13]))]
!  o2 = reshape([acontainer(1,2)(dka(1)(1), dla(2)('qq',[-1,-2])), acontainer(1,2)(dka(1)(1), dla(2)('qq',[-1,-2]))], [1,2])
!
  print *, lbound(o1), ":", ubound(o1), "::", 1, ":", 2
  print *, lbound(o2), ":", ubound(o2), "::", 1,1, ":", 1,2
  if (any(lbound(o1)/=1) .or. any(ubound(o1)/=[2]) &
      .or. any(lbound(o2)/=1) .or. any(ubound(o2)/=[1,2])) stop 6


!  v1a = [(acontainer(4,5)(dka(4)(100+i),dla(5)('aaaaa',[(i+j,j=1,5)])), i=1,size(v1a))]
  do i = 1, size(v1a)
    v1aa(i) = acontainer(4,5)(dka(4)(100+i),dla(5)('aaaaa',[(i+j,j=1,5)]))
  end do
  v1a = v1aa

!  v1b = [(acontainer(4,5)(dka(4)(-100+i),dla(5)('bbbbb',[(i+2*j,j=1,5)])), i=lbound(v1b,1),ubound(v1b,1))]
  do i = lbound(v1b,1),ubound(v1b,1)
    v1bb(i) = acontainer(4,5)(dka(4)(-100+i),dla(5)('bbbbb',[(i+2*j,j=1,5)]))
  end do
  v1b = v1bb

  v3a = acontainer(4,5)(dka(4)(49),dla(5)('ccccc',[1,2,3,4,5]))
!  v3b = reshape([(acontainer(4,5)(dka(4)(10+i), dla(5)('ddddd', [(i-j,j=1,5)])), i=1,8)], [2,2,2])

  do i = 1, 8
    v3bb(i) = acontainer(4,5)(dka(4)(10+i), dla(5)('ddddd', [(i-j,j=1,5)]))
  end do
  v3b = reshape(v3bb, [2,2,2])

  v0  = acontainer(4,4)(dka(4)(99),dla(4)('eeee',[2,3,4,5]))

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

!  v1 = [acontainer(4,5)(dka(4)(1),dla(5)('fffff',five)), acontainer(4,5)(dka(4)(2),dla(5)('ggggg',2*five)), acontainer(4,5)(dka(4)(3),dla(5)('hhhhh',3*five))]

  v11(1) = acontainer(4,5)(dka(4)(1),dla(5)('fffff',five))
  v11(2) = acontainer(4,5)(dka(4)(2),dla(5)('ggggg',2*five))
  v11(3) = acontainer(4,5)(dka(4)(3),dla(5)('hhhhh',3*five))
  v1 = v11

!  v3 = reshape([(acontainer(4,5)(dka(4)(i),dla(5)('kkkkk',4*five)), i=1,24)], [4,2,3])
  do i = 1, 24
    v33(i) = acontainer(4,5)(dka(4)(i),dla(5)('kkkkk',4*five))
  end do
  v3 = reshape(v33, [4,2,3])

  print *, lbound(v1), ":", ubound(v1), "::", 1, ":", 3
  print *, lbound(v3), ":", ubound(v3), "::", 1,1,1, ":", 4,2,3
  if (any(lbound(v1)/=1) .or. any(ubound(v1)/=[3]) &
      .or. any(lbound(v3)/=1) .or. any(ubound(v3)/=[4,2,3])) stop 10

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (.not.(allocated(o1) .and. allocated(o2) .and. allocated(v1) .and. allocated(v3))) stop 10

end program dtpIAABounds006
