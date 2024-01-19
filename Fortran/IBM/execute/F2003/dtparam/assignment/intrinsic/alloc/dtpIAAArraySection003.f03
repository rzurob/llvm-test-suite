!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (container) via intrinsic assignment and check lower and upper bounds - array sections as source
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABounds003 (<-dtpIAAArray003<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  An allocatable DTP array with a kind and a length parameter, assigned to from
!*  an array section - verify that the bounds are correct.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArraySection003mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar
  end type dk

  type dl(l)
     integer, len :: l
     character(l) :: chvar
     integer :: iarr(l)
  end type dl

  type container(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dk(k)) :: dkvar
     type(dl(l)) :: dlvar
  end type container

end module dtpIAAArraySection003mod



program dtpIAAArraySection003

  use dtpIAAArraySection003mod
  implicit none

  type(container(1,:)), allocatable :: o1(:), o2(:,:)
  type(container(1,2)) :: o1a(3), o1b(-4:-1), o2a(1,2), o2b(-1003:-1001,10:11)
  type(container(4,:)), allocatable :: v1(:), v3(:,:,:)
  type(container(4,5)) :: v1a(2), v1b(1:2), v3a(1,1,1), v3b(4:5,3:4,2:3)

  integer :: i

  ! create arrays o1a .. a2b and assign sections of them to allocatable arrays, and then check the bounds
  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (allocated(o1) .or. allocated(o2) .or. allocated(v1) .or. allocated(v3)) error stop 2

  o1a = [(container(1,2)(dk(1)(100+i), dl(2)(repeat(achar(99+i),2),[50+i,500+i])), i=1,size(o1a))]
  o1b = [(container(1,2)(dk(1)(10+i), dl(2)(repeat(achar(69+i),2),[550+i,5500+i])), i=lbound(o1b,1),ubound(o1b,1))]

  o2a = reshape([container(1,2)(dk(1)(49), dl(2)('xy',[1,2])), container(1,2)(dk(1)(50), dl(2)('za',[2,1]))], [1,2])
  o2b = reshape([(container(1,2)(dk(1)(10+i), dl(2)('xx',[10+i,11+i])), i=1,6)], [3,2])

  o1 = o1a(2:3)
  o2 = o2a(:,:)

  print *, lbound(o1), ":", ubound(o1), "::", 1, ":", 2
  print *, lbound(o2), ":", ubound(o2), "::", [1,1], ":", [1,2]
  if (any(lbound(o1)/=1) .or. any(ubound(o1)/=2) &
      .or. any(lbound(o2)/=[1,1]) .or. any(ubound(o2)/=[1,2])) error stop 3

  o1 = o1b(:)
  o2 = o2b(:-1002,:)

  print *, lbound(o1), ":", ubound(o1), "::", 1, ":", 4
  print *, lbound(o2), ":", ubound(o2), "::", [1,1], ":", [2,2]
  if (any(lbound(o1)/=1) .or. any(ubound(o1)/=4) &
      .or. any(lbound(o2)/=[1,1]) .or. any(ubound(o2)/=[2,2])) error stop 4


  v1a = [(container(4,5)(dk(4)(100+i),dl(5)('aaaaa',i)), i=1,size(v1a))]
  v1b = [(container(4,5)(dk(4)(-100+i),dl(5)('bbbbb',i)), i=lbound(v1b,1),ubound(v1b,1))]

  v3a = container(4,5)(dk(4)(49),dl(5)('ccccc',[1,2,3,4,5]))
  v3b = reshape([(container(4,5)(dk(4)(10+i), dl(5)('ddddd', -i)), i=1,8)], [2,2,2])

  v1 = v1a(1:)
  v3 = v3a(1:,:,1:)

  print *, lbound(v1), ":", ubound(v1), "::", 1, ":", 2
  Print *, lbound(v3), ":", ubound(v3), "::", [1,1,1], ":", [1,1,1]
  if (any(lbound(v1)/=1) .or. any(ubound(v1)/=2) &
      .or. any(lbound(v3)/=[1,1,1]) .or. any(ubound(v3)/=[1,1,1])) error stop 7

  v1 = v1b(:2)
  v3 = v3b(:5,:4,:)

  print *, lbound(v1), ":", ubound(v1), "::", 1, ":", 2
  print *, lbound(v3), ":", ubound(v3), "::", [1,1,1], ":", [2,2,2]
  if (any(lbound(v1)/=1) .or. any(ubound(v1)/=2) &
      .or. any(lbound(v3)/=[1,1,1]) .or. any(ubound(v3)/=[2,2,2])) error stop 8

  print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
  if (.not.(allocated(o1) .and. allocated(o2) .and. allocated(v1) .and. allocated(v3))) error stop 10

end program dtpIAAArraySection003
