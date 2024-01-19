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
!*  DESCRIPTION                : This_image() should always produce a result
!*                               less than or equal to num_images().
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	real, save :: caf[*]

contains

	integer function fun1(x)
		integer :: x

		fun1 = this_image()
		if (fun1 > x) then
			print *, fun1, x
			error stop 14
		end if
	end function

end module


program main
	use modFDC
	integer :: num

	if (this_image() > num_images()) then
		print *, this_image(), num_images()
		error stop 11
	end if

	num = fun0()
	if (num > num_images()) then
		print *, num, num_images()
		error stop 12
	end if

	call sub1(num)
	num = fun1(num_images())

contains

	function fun0()
		integer :: fun0

		fun0 = this_image()
	end function

end


subroutine sub1(n)
	integer :: n
	n = this_image()

	if (n > num_images()) then
		print *, n, num_images()
		error stop 13
	end if
end subroutine
