!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : num_images_f001.f
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
!*  DESCRIPTION                : Num_images produces a value greater than 1.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modXYZ
contains
	integer function fun2()
		fun2 = (num_images() * num_images()) / num_images()
	end function
end module


program main
	use modXYZ
	integer*8, save :: caf[*]
	integer, parameter :: n = 3
	integer :: num(n)

	interface
		subroutine sub1(c)
			integer*8 :: c[*]
		end subroutine
	end interface
	
	num(1) = num_images()
	num(2) = fun1()
	num(3) = fun2()
	
	do i = 1, n
		if (num(i) < 1) then
        	        print *, num(i)
                	error stop 11
	        end if
	end do

	call sub1(caf)
	
contains

	integer function fun1()
		fun1 = num_images()
	end function

end


subroutine sub1(coa)
	integer*8 :: coa[*]

	coa = num_images()
	if (coa < 1) then
		print *, num_images(), coa
		error stop 12
	end if
end subroutine
