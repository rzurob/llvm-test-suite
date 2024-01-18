!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : image_index_d008.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : August 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Image_index requires the 2nd arg be a rank-1
!*				 integer array with size equal to the corank.
!*				 Test this with deferred shape arrays as 2nd arg.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real(8), save :: caf1[*]
	complex(8), save :: caf2[1:2,0:2,-1:*]
	integer, allocatable :: arr1(:)
	integer, pointer :: arr2(:)
	
	allocate(arr1(1))
	arr1 = 10
	print *, image_index(caf1, arr1)
	deallocate(arr1)
	sync all
	
	allocate(arr1(3))
	arr1 = [2, 1, -1]
	print *, image_index(caf2, arr1)
	deallocate(arr1)
	sync all
	
	allocate(arr1(3))
	arr1 = [1, 2, 0]
	print *, image_index(caf2, arr1)
	deallocate(arr1)
	sync all
	
	
	allocate(arr2(1))
	arr2 = 5
	print *, image_index(caf1, arr2)
	deallocate(arr2)
	sync all
	
	allocate(arr2(3))
	arr2 = [1, 1, 0]
	print *, image_index(caf2, arr2)
	deallocate(arr2)
	sync all
	
end
