!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This_image(caf) should always produce a rank 1
!*                               array result whereby each cobound should be greater
!*                               than 1 if none are defined at declaration.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	integer, allocatable :: arr2(:)

contains

	subroutine sub2(caf, x)
		integer :: x
		real :: caf[1,1,*]

		allocate(arr2(2))
		arr2 = this_image(caf)

		if ( any(arr2 .lt. [1,1,1]) ) then
			print *, arr2
			error stop 14
		end if

		deallocate(arr2)
		sync all
	end subroutine

end module


program main
	use modFDC
	real, save :: caf[*]
	integer, allocatable :: arr1(:)

	allocate(arr1(1))
	arr1 = this_image(caf)

	if ( any(arr1 .lt. [1]) ) then
		print *, arr1
		error stop 11
	end if

	deallocate(arr1)
	sync all

	call sub0()
	call sub1(caf)
	call sub2(caf, num_images())

contains

	subroutine sub0()
		logical, save :: caf2[0:1,-1:*]

		allocate(arr1(2))
		arr1 = this_image(caf2)

		if ( any(arr1 .lt. [0,-1]) ) then
			print *, arr1
			error stop 12
		end if

		deallocate(arr1)
		sync all
	end subroutine
end


subroutine sub1(caf)
	integer, allocatable :: arr3(:)
	real :: caf[2,*]

	allocate(arr3(2))
	arr3 = this_image(caf)

	if ( any(arr3 .lt. [1,1]) ) then
		print *, arr3
		error stop 13
	end if

	deallocate(arr3)
	sync all
end subroutine
