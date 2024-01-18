!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : this_image_f002b.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : July 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This_image(caf) should always produce a rank 1 array
!*                               result whereby each cobound should be less than or
!*                               equal to num_images() if none are defined at declaration.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	integer, allocatable :: arr(:)

contains

	subroutine sub2(x)
		integer :: x
		integer, save :: caf[0:*]
	
		x = x - 1
		allocate(arr(1))
		arr = this_image(caf)

		if ( any(arr .gt. [x]) ) then
			print *, arr, x
			error stop 13
		end if
		deallocate(arr)
		sync all
	end subroutine

end module


program main
	use modFDC
	integer :: num
	real, save :: caf[*]

	num = num_images()

	allocate(arr(1))
	arr = this_image(caf)
	if ( any(arr .gt. [num]) ) then
		print *, arr, num
		error stop 11
	end if
	deallocate(arr)
	sync all

	call sub1()
	call sub2(num_images())
end


subroutine sub1()
	integer :: num
	integer, allocatable :: arr(:)
	complex, save :: caf[1,1,0:1,1:*]

	num = num_images()
	allocate(arr(4))
	arr = this_image(caf)

	if ( any(arr .gt. [num,num,num,num]) ) then
		print *, arr, num
		error stop 12
	end if
	
	deallocate(arr)
	sync all
	
end subroutine
