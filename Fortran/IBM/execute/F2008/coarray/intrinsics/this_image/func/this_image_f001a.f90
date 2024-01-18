!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This_image() should always produce a result
!*                               greater than 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	real, save :: caf[*]

contains

	integer function fun1(x)
		integer :: x

		do i = 1, x
			fun1 = this_image()
			if (fun1 < 1) then
				print *, fun1, i
				error stop 14
			end if
		end do
	end function

end module


program main
	use modFDC
	integer :: num

	if (this_image() < 1) then
		print *, this_image()
		error stop 11
	end if

	num = fun0()
	if (num < 1) then
		print *, num
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

	if (n < 1) then
		print *, n
		error stop 13
	end if
end subroutine
