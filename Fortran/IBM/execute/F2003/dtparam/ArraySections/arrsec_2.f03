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
!*  dimension unless the sequence is empty. The array section is the set of
!*  elements from the array determined by all possible subscript lists
!*  obtainable from the single subscripts or sequences of subscripts specified
!*  by each section subscript.
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

! -----------------
! subscript-triplet:
! -----------------

print *,'subscript-triplet tests:'

! fixed-shape array within bounds:
dta(1:4:1)%id = (/1,2,3,4/)
dta(:)%id = (/6,7,8,9/)
print *,size(dta(1:4)%id)
print *,size(dta(::1)%id)
print *,dta(:)%id
print *,dta(1:1:4)%id
print *,dta(1:2:4)%id
print *,dta(1:3:2)%id
print *,dta(1:4:3)%id

! deferred-shape array within bounds:
dtb(1:4:1)%id = (/1,2,3,4/)
dtb(:)%id = (/6,7,8,9/)
print *,size(dtb(1:4)%id)
print *,size(dtb(::1)%id)
print *,dtb(:)%id
print *,dtb(1:1:4)%id
print *,dtb(1:2:4)%id
print *,dtb(1:3:2)%id
print *,dtb(1:4:3)%id

! deferred-shape array out of bounds:
print *,'next 3 statements should trap with -C'
dtb(1:5:1)%id = (/1,2,3,4,5/)
dtb(:)%id = (/5,6,7,8,9/)
print *,size(dtb(1:5)%id)
print *,dtb(:)%id
print *,dtb(1:1:4)%id
print *,dtb(1:2:4)%id
print *,dtb(1:3:2)%id
print *,dtb(1:4:3)%id

! empty sequence:
print *, dta(50:10)%id
print *, dtb(50:10)%id
print *, size(dta(20:30:-1)%id)
print *, size(dtb(20:30:-1)%id)

! -----------------
! vector-subscript:
! -----------------
print *,'vector subscript tests:'

! fixed-shape array within bounds:
dta((/1,2,3,4/))%id = (/1,2,3,4/)
print *,size(dta((/1,2,3,4/))%id)
print *,dta((/1,2,3,4/))%id
print *,dta(:)%id
print *,dta((/1/))%id
print *,dta((/1,3/))%id
print *,dta((/4,2,1,3/))%id

! deferred-shape array within bounds:
dtb((/1,2,3,4/))%id = (/1,2,3,4/)
print *,size(dtb((/1,2,3,4/))%id)
print *,dtb((/1,2,3,4/))%id
dtb(:)%id = dtb((/1,2,3,4/))%id
print *,size(dtb(:)%id)
print *,dtb(:)%id
print *,dtb((/1/))%id
print *,dtb((/1,3/))%id
print *,dtb((/4,2,1,3/))%id

! deferred-shape array out of bounds:
print *,'next 3 statements should trap with -C'
dtb((/1,2,3,4,5/))%id = (/1,2,3,4,5/)
print *,size(dtb((/1,2,3,4,5/))%id)
print *,dtb((/1,2,3,4,5/))%id

end
