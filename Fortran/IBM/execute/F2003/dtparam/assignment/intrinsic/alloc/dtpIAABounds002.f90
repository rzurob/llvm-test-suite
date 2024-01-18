!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABounds002
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (len) via intrinsic assignment and check lower and upper bounds
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAAArray002 (<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a length parameter - we create arrays
!*  with specific bounds, and verify that they are allocated correctly.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABounds002mod

  implicit none
  type dl(l)
     integer, len :: l
     character(l) :: chvar
     integer      :: ivar(l)
  end type dl

end module dtpIAABounds002mod



program dtpIAABounds002

  use dtpIAABounds002mod
  implicit none

  type(dl(:)), allocatable :: v1(:), v2(:,:), v3(:,:,:)
  type(dl(3)) :: v1a(3), v1b(-2:1), v2a(2,1), v2b(0:1,-3:-1), v3a(1,1,1), v3b(2:3,3:4,4:6)
  type(dl(4)) :: v0

  integer :: i

  print *, allocated(v1), allocated(v2), allocated(v3)
  if (allocated(v1) .or. allocated(v2) .or. allocated(v3)) stop 2

  v1a = [dl(3)('abc',[1234321,23432,-1234321]), dl(3)('def',[2345432,34543,-2345432]), dl(3)('ghi',[3456543,45654,-3456543])]
  v1b = [dl(3)('jkl',[1221,22,-1221]), dl(3)('mno',[2332,33,-2332]), dl(3)('pqr',[3443,454,-3443]), dl(3)('stu',[4554,55,-4554])]

  v2a = reshape([dl(3)('wxy',[23432,-23432,34321]), dl(3)('zyx',[345432,-4543,-34543])],[2,1])
  v2b = reshape([(dl(3)(repeat(achar(64+i),3),[i*10+1,i*10+2,i*10+3]), i=1,6)], [2,3])

  v3a = dl(3)('xxx',[3,27,81])
  v3b = reshape([(dl(3)(repeat(achar(96+i),3),[i*100+1,i*100+2,i*100+3]), i=1,12)], [2,2,3])

  v0 = dl(4)('Earl',[69, 97, 114, 108])

  v1 = v1a
  v2 = v2a
  v3 = v3a

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1a), ":", ubound(v1a)
  print *, lbound(v2), ":", ubound(v2), "::", lbound(v2a), ":", ubound(v2a)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3a), ":", ubound(v3a)
  if (any(lbound(v1)/=lbound(v1a)) .or. any(ubound(v1)/=ubound(v1a)) &
      .or. any(lbound(v2)/=lbound(v2a)) .or. any(ubound(v2)/=ubound(v2a)) &
      .or. any(lbound(v3)/=lbound(v3a)) .or. any(ubound(v3)/=ubound(v3a))) stop 3

  v1 = v1b
  v2 = v2b
  v3 = v3b

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
  print *, lbound(v2), ":", ubound(v2), "::", lbound(v2b), ":", ubound(v2b)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
  if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
      .or. any(lbound(v2)/=lbound(v2b)) .or. any(ubound(v2)/=ubound(v2b)) &
      .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) stop 4

  v1 = v0
  v2 = v0
  v3 = v0

  print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
  print *, lbound(v2), ":", ubound(v2), "::", lbound(v2b), ":", ubound(v2b)
  print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
  if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
      .or. any(lbound(v2)/=lbound(v2b)) .or. any(ubound(v2)/=ubound(v2b)) &
      .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) stop 5

  v1 = [dl(0)('',[integer::]), dl(0)('',[integer::]), dl(0)('',[integer::]), dl(0)('',[integer::])]
  v2 = reshape([dl(5)('abcde',1), dl(5)('bcdef',2), dl(5)('cdefg',3), dl(5)('defgh',4)],[4,1])
  v3 = reshape([dl(1)('a',[22]), dl(1)('b',[42]), dl(1)('c',[66]), dl(1)('d',[26]), dl(1)('e',[88]), dl(1)('f',[99])],[1,2,3])

  print *, lbound(v1), ":", ubound(v1), "::", 1, ":", 4
  print *, lbound(v2), ":", ubound(v2), "::", 1,1, ":", 4,1
  print *, lbound(v3), ":", ubound(v3), "::", 1,1,1, ":", 1,2,3
  if (any(lbound(v1)/=1) .or. any(ubound(v1)/=4) &
      .or. any(lbound(v2)/=1) .or. any(ubound(v2)/=[4,1]) &
      .or. any(lbound(v3)/=1) .or. any(ubound(v3)/=[1,2,3])) stop 6

  print *, allocated(v1), allocated(v2), allocated(v3)
  if (.not.(allocated(v1) .and. allocated(v2) .and. allocated(v3))) stop 7

end program dtpIAABounds002
