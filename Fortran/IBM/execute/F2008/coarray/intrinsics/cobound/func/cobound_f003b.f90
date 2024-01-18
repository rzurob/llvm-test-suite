!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_f003b.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : September 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with coarrays
!*                               having very large coranks using 2 arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	logical, save :: caf1(1,2,3,4,5,6,7,8,9)[2,2,2,2,2,2,2,2,2,2:*]
	logical, save :: caf2[1:19,18,17,16,15,14,13,1:12,11,0:9,-1:7,8,7,6,5,4,3,2,1,0:*]
	integer, allocatable :: arr1(:), arr2(:)
	integer :: size
	
	
	size = 10
	allocate(arr1(size), arr2(size))
	do i = 1, size
		arr1(i) = lcobound(caf1, i)
		arr2(i) = ucobound(caf1, i)
	end do
	print *, arr1, "::", arr2
	deallocate(arr1, arr2)
	sync all
	
	
	size = 20
	allocate(arr1(size), arr2(size))
	do i = 1, size
		arr1(i) = lcobound(caf2, i)
		arr2(i) = ucobound(caf2, i)
	end do
	print *, arr1, "::", arr2
	deallocate(arr1, arr2)
	sync all
	
end
