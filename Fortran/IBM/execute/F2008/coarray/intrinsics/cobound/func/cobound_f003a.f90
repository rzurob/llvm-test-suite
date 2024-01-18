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
!*                               having very large coranks using 1 argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	real, save :: caf1(1,2,3,4,5,6,7,8,9)[1,2,3,4,5,6,7,8,9,*]
	real, save :: caf2[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,*]
	real, save :: caf3[1:2,0:1,9:10,10000:10001,-100:-99,*]
	integer, allocatable :: arr1(:), arr2(:)


	allocate(arr1(10), arr2(10))
	arr1 = lcobound(caf1)
	arr2 = ucobound(caf1)

	if ( any(arr1 .ne. [1,1,1,1,1,1,1,1,1,1]) ) then
		print *, arr1
		error stop 11
	end if
	if ( any(arr2 .ne. [1,2,3,4,5,6,7,8,9,1]) ) then
		print *, arr2
		error stop 12
	end if
	deallocate(arr1, arr2)
	sync all


	allocate(arr1(20), arr2(20))
	arr1 = lcobound(caf2)
	arr2 = ucobound(caf2)

	if ( any(arr1 .ne. [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) then
		print *, arr1
		error stop 13
	end if
	if ( any(arr2 .ne. [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,1]) ) then
		print *, arr2
		error stop 14
	end if
	deallocate(arr1, arr2)
	sync all


	allocate(arr1(6), arr2(6))
	arr1 = lcobound(caf3)
	arr2 = ucobound(caf3)

	if ( any(arr1 .ne. [1,0,9,10000,-100,1]) ) then
		print *, arr1
		error stop 15
	end if
	if ( any(arr2 .ne. [2,1,10,10001,-99,1]) ) then
		print *, arr2
		error stop 16
	end if
	deallocate(arr1, arr2)
	sync all

end
