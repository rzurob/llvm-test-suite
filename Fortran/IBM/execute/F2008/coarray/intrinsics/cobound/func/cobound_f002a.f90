!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with coarrays
!*                               of various corank using 1 argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf1[*], caf2[2,*], caf3[2,2,*], caf4[-10:-9,1,2,*]
	integer, save :: caf5[-1:-1,-1:-1,-1:-1,-1:-1,*], caf6[2,2,2,1,1,0:*]
	integer, save :: caf7[2,1:1,1,0:3,1,1,*], caf8[9:9,10:10,11:11,12:12,13:14,15:16,17:18,19:*]

	integer, allocatable :: arr1(:), arr2(:)

!##### CORANK = 1
	allocate(arr1(1), arr2(1))
	arr1 = lcobound(caf1)
	arr2 = ucobound(caf1)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all

!##### CORANK = 2
	allocate(arr1(2), arr2(2))
	arr1 = lcobound(caf2)
	arr2 = ucobound(caf2)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all

!##### CORANK = 3
	allocate(arr1(3), arr2(3))
	arr1 = lcobound(caf3)
	arr2 = ucobound(caf3)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all

!##### CORANK = 4
	allocate(arr1(4), arr2(4))
	arr1 = lcobound(caf4)
	arr2 = ucobound(caf4)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all

!##### CORANK = 5
	allocate(arr1(5), arr2(5))
	arr1 = lcobound(caf5)
	arr2 = ucobound(caf5)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all

!##### CORANK = 6
	allocate(arr1(6), arr2(6))
	arr1 = lcobound(caf6)
	arr2 = ucobound(caf6)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all

!##### CORANK = 7
	allocate(arr1(7), arr2(7))
	arr1 = lcobound(caf7)
	arr2 = ucobound(caf7)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all

!##### CORANK = 8
	allocate(arr1(8), arr2(8))
	arr1 = lcobound(caf8)
	arr2 = ucobound(caf8)
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)

end
