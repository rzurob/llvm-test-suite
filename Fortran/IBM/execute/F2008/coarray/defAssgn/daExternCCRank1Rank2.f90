!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : daExternCCRank1Rank2
!*
!*  DATE                       : 2010-12-20
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 CAF coarray defined assignment
!*  SECONDARY FUNCTIONS TESTED : External routine with both args coarrays of matching type, but mismatched rank
!* keywords:	ext	1/2	dcc	acc	inout	c-ext	var
!*
!*  DESCRIPTION
!*
!*  External routine with both args coarrays of matching type, but mismatched rank.
!*  No sync is required, since none of the coarray variables depend on results from other images.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program daExternCCRank1Rank2
  implicit none
  interface
     subroutine workhorse(v1,v2)
       integer :: v1(:)[*]
       integer :: v2(:,:)[*]
     end subroutine workhorse
  end interface
  integer, save :: c1(3)[*], c2(3,4)[*]
  call workhorse(c1, c2)
end program daExternCCRank1Rank2

subroutine workhorse(v1,v2)
  implicit none
  integer :: v1(:)[*]
  integer :: v2(:,:)[*]

  interface assignment ( = )
     subroutine intAssign(x, y)
       integer, intent(inout) :: x(:)[*]
       integer, intent(in) :: y(:,:)[*]
     end subroutine intAssign
  end interface assignment ( = )

  integer :: i, j, me

  ! intrinsic assignment to start:
  me = this_image()
  v1 = [(10000*i+me*1000, i=1,3)]
  v2 = reshape([((j*100+i*10+me,j=1,3),i=1,4)],[3,4])
  if (me == 1) print *, v1, v2
  ! now do the defined assignment:
  v1 = v2
  if (me == 1) print *, v1, v2
  if (any(v2 /= reshape([((j*100+i*10+me,j=1,3),i=1,4)],[3,4]))) error stop 2
  if (any(v1 /= [(10000*i+me*1000 + sum(v2(i,:)), i=1,3)])) error stop 3

end subroutine workhorse

subroutine intAssign(x, y)
  implicit none
  integer, intent(inout) :: x(:)[*]
  integer, intent(in) :: y(:,:)[*]
  integer :: i
  do i = 1, size(x)
     x(i) = x(i) + sum(y(i,:))
  end do
end subroutine intAssign
