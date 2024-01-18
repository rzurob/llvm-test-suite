!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : February 2011
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test critical construct with a derived type
!*                               coarray local to the compilation unit.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer :: n, cur

	n = num_images()
	cur = this_image()
	call sub0(n, cur)

contains

	subroutine sub0(num, me)
		integer :: num, me

		type dt
			integer :: x, y
		end type

		type (dt), save :: cafdt[1,1:2,1:*]
		cafdt = dt(1, 1)
		sync all

		critical
			cafdt[1,1,1]%x = cafdt[1,1,1]%x * (-1)
			!cafdt[1,1,2]%y = cafdt[1,2,1]%x
			cafdt[1,2,1]%x = cafdt[1,1,2]%x + cafdt[1,1,2]%y + cafdt[1,2,1]%x
			cafdt[1,2,2] = dt(9, 10)
		end critical

		sync all

		if (this_image() == 1) then
			print *, cafdt[1,1,1]		!(1, 1)
			print *, cafdt[1,2,1]		!(17, 15)
			print *, cafdt[1,1,2]		!(1, 1)
			print *, cafdt[1,2,2]		!(9, 10)
		end if
	end subroutine

end
