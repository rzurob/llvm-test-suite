!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_1.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Jul. 09, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3 Array sections
!*  9-10:
!*  In an array-section having a section-subscript-list, each subscript-triplet 
!*  and vector-subscript in the section subscript list indicates a sequence of 
!*  subscripts, which may be empty.
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
type(base(:)), allocatable :: dtarrobj(:)
allocate(base(2) :: dtarrobj(5))

! subscript-triplet:
dtarrobj(1:5:1)%id = (/1,2,3,4,5/)
print *, dtarrobj(1:5:1)%id
print *, dtarrobj(:)%id

dtarrobj(:)%id = (/6,7,8,9,10/)
dtarrobj(4:1:-2)%id = (/1,2/)
print *, dtarrobj(:)%id
! empty:
print *, size(dtarrobj(4:1:2)%id)
print *, dtarrobj(4:2:1)%id

! vector-subscript:
dtarrobj((/1,2,3,4,5/))%id = (/1,2,3,4,5/)
print *,dtarrobj%id
print *,dtarrobj(:)%id

dtarrobj((/3,1,4,2,5/))%id = (/8,6,9,7,5/)
print *,dtarrobj(:)%id
end
