!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  NOTES:
!*  This source file is used by both arrsec_2.scenario and arrsec_2d1.scenario
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3 Array sections
!*  10-13:
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

! deferred-shape array out of bounds:
print *,'next statement should trap with -C'

dta((/1,2,3,4,5/))%id = (/1,2,3,4,5/)
print *,dta((/1,2,3,4,5/))%id

dtb((/1,2,3,4,5/))%id = (/1,2,3,4,5/)
print *,dtb((/1,2,3,4,5/))%id

end
