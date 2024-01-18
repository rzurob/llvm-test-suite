!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_3.f
!*  DATE                       : Jul. 09, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3 Array sections
!*  14-3:
!*  In an array-section with no section-subscript-list, the rank and shape
!*  of the array is the rank and shape of the part-ref with nonzero rank;
!*  otherwise (a) the rank of the array section is the number of subscript
!*  triplets and vector subscripts in the section subscript list.
!*  (b) The shape is the rank-one array whose ith element is the number of
!*  integer values in the sequence indicated by the ith subscript triplet or
!*  vector subscript. If any of these sequences is empty, the array section has
!*  size zero. The subscript order of the elements of an array section is that
!*  of the array data object that the array section represents.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module m
	type base(n1)
		integer, len :: n1
		integer :: arr(n1)
		integer :: id
	end type

	type dt1(n1)
		integer, len :: n1
		integer :: id1
		integer :: arr1(n1)
	end type

	type, extends(dt1) :: dt2(n2)
		integer, len :: n2
		integer :: arr2(n2)
		integer :: id2
		type(base(n2)) :: bc(n2,n2*2)
	end type
end module

use m
integer, parameter :: N = 3
type(dt2(N,N)) :: dta(N)

! In an array section...
dta%dt1%id1 = (/(i,i=1,N)/)
print *,'dta%dt1%id=',dta%dt1%id1
print *,'size(dta%dt1)=',size(dta%dt1)

dta(1)%bc%id = reshape((/(i,i=1,N*N*2)/),(/N,N*2/))
print *,'dta(1)%bc%id=',dta(1)%bc%id
print *,'size(dta(1)%bc%id)=',size(dta(1)%bc%id)

! otherwise...
! 1 vector subscript + 1 subscript triplet = rank 2 array section
! of shape = (/3,1:N*2/); extent = 3*N*2
print *,dta(1)%bc((/1,2,3/),1:N*2:2)%id
print *,size(dta(1)%bc((/1,2,3/),1:N*2:2)%id)
do j=1,N*2,2
	do i=1,3
		print *,dta(1)%bc(i,j)%id
	end do
end do

! If any of these sequences is empty, the array section has size zero:
print *,dta(1)%bc((/1,2,3/),N:1)%id
print *,size(dta(1)%bc((/1,2,3/),N:1)%id)

end
