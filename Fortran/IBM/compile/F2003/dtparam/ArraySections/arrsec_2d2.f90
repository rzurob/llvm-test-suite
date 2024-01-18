!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_2d2.f
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3 Array sections
!*  10-11:
!*  Each subscript in such a sequence shall be within the bounds for its
!*  dimension unless the sequence is empty.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
	type base(n)
		integer, len :: n
		integer :: id
		integer :: arr(n)
	end type
end module

use m
type(base(2)) :: dta(4)
type(base(:)), allocatable :: dtb(:)
allocate(base(2) :: dtb(4))

! subscript-triplet:

! fixed-shape array out of bounds:
dta(1:5:1)%id = (/1,2,3,4,5/)
dta(:)%id = (/5,6,7,8,9/)

! deferred-shape array out of bounds:
dtb(1:5:1)%id = (/1,2,3,4,5/)
dtb(:)%id = (/5,6,7,8,9/)

end
