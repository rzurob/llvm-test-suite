!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : January 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test calling a user function inside a critical construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer*8, save :: x[*]
	integer*8 :: values(8), f

	x = 2
	values = [10, 42, 170, 682, 2730, 10922, 43690, 174762]
	sync all

	if (num_images() <= 8) then
		critical
			call sub0(x)
			f = fun1()
		end critical

		if ( x[1] /= values(num_images()) ) then
			print *, "actual", x[1]
			print *, "expected", values(num_images())
			error stop 33
		end if
	end if

contains

	subroutine sub0(x)
		integer*8 :: x[*]
		integer*8 :: tmp, tmp2, tmp3

		tmp = x[1] + 1
		tmp2 = tmp + x[1]
		call sleep_(1)
		tmp3 = tmp2 * x[2]
		x[1] = tmp3
	end subroutine

end


integer function fun1()
	integer, save :: caf[*]
	integer :: tmp, tmp2, tmp3

	caf = 9

	tmp = caf[1] + 1
	tmp2 = tmp + caf[1]
	tmp3 = tmp2 * caf[2]
	caf[1] = tmp3

	fun1 = 1
end function


