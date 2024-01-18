!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_5.f
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3.1 Subscript triplet, 11-17:
!*  When the stride is positive, the subscripts specified by a triplet form a
!*  regularly spaced sequence of integers beginning with the first subscript and
!*  proceeding in increments of the stride to the largest such integer not
!*  greater than the second subscript; the sequence is empty if the first
!*  subscript is greater than the second.
!*
!*  When the stride is negative, the sequence begins with the first subscript
!*  and proceeds in increments of the stride down to the smallest such integer
!*  equal to or greater than the second subscript; the sequence is empty if the
!*  second subscript is greater than the first.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
	type base(n)
		integer, len :: n
 		integer :: arr(n)
		integer :: id
	end type
end module

use m
type(base(:)), allocatable :: carr(:,:)
allocate(base(4) :: carr(3,2))

carr(2:3,1:2)%id = reshape([1,2,3,4],[2,2])
! currently outputs 0 0 0 0 instead of 1 2 3 4 (fixed in feature 337402)
print *,carr(2:3,1:2)%id
print *,carr(3:2,1:2)%id
print *,carr(3:2,1:2)%id
print *,carr(2:3,2:1)%id
print *,carr(3:2:-1,1:2)%id
print *,carr(3:2:-1,2:1:-2)%id
print *,carr(2:3:3,1:0:-2)%id
end
