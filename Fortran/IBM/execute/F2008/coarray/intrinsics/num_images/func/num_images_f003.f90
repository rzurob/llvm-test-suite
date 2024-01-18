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
!*  DESCRIPTION                : Num_images produces same result consistently.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modXYZ
contains
	integer function fun2()
		fun2 = (num_images() + num_images()) - num_images()
	end function
end module


program main
	use modXYZ
	integer, save :: caf[*]
	integer :: x, y, z

	interface
		subroutine sub1(c, n)
			integer, codimension[*] :: c
			integer :: n
		end subroutine
	end interface

	x = num_images()
	y = fun1()
	z = fun2()

	if (x /= y) then
		print *, x, y
		error stop 11
	end if
	if (y /= z) then
		print *, y, z
		error stop 12
	end if

	call sub1(caf, x)

contains

	integer function fun1()
		fun1 = num_images()
	end function

end


subroutine sub1(coa, num)
	integer, codimension[*] :: coa
	integer :: num

	coa = num_images()
	if (coa /= num) then
		print *, num_images(), coa, num
		error stop 13
	end if
end subroutine
