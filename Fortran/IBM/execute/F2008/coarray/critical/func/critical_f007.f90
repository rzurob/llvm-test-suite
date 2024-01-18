!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f007.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : January 2011
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test critical construct containing a call to
!*                               an internal procedure that uses local coarrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: num

	num = 100000
	
	if (num_images() > 2) then
		call sub0(num)
	else
		print *, "too few images, test didn't run"
	end if
	
contains

	subroutine sub0(n)
		integer :: n, x
		integer, save :: caf1[*], caf2[*]
		
		x = 2011
		caf1 = 0
		caf2 = 0
		sync all

		do i = 1, n
			one: critical
				if (this_image() == 1) then
					caf1[2] = x
					caf2[2] = x
				else
					if (caf1[2] /= caf2[2]) then
						print *, caf1[2], caf2[2], i
						error stop 15
					end if
				end if
			end critical one			
		end do
	end subroutine
end
