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
!*  DESCRIPTION                : This_image(caf, x) should always produce a result
!*                               greater than 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, allocatable :: lb(:), ub(:), arr(:)
	real, save :: caf[*]

	allocate(arr(1))
	arr = this_image(caf, 1)
	if ( any(arr .lt. [1]) ) then
		print *, arr
		error stop 11
	end if
	deallocate(arr)
	sync all

	call sub0()
	call sub1(caf)

contains

	subroutine sub0()
		complex, save :: caf2[2,0:1,-1:*]
		integer, allocatable :: lb(:), ub(:)
		integer :: imval, num

		num = 1
		lb = lcobound(caf2)
		ub = ucobound(caf2)

		if (size(lb) /= size(ub)) then
			print *, lb, ":", ub
			error stop 12
		end if

		do i = 1, size(lb)
			imval = this_image(caf2, i)
			if (imval < num) then
				print *, imval, num, this_image()
				error stop 13
			end if

			num = num - 1
		end do
		sync all
	end subroutine
end


subroutine sub1(caf)
	real :: caf[-8:*]
	integer :: arr(1)

	arr = this_image(caf, 1)
	if ( any(arr .lt. [-8]) ) then
		print *, arr
		error stop 14
	end if
	sync all

end subroutine
