!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f004.f
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
!*  DESCRIPTION                : Test critical construct with global complex coarrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module muse
	implicit none
	complex(4), save :: caf(0:98)[0:*]
end module


program main
	use muse
	complex(4), parameter :: num = (-5.5, 0.75)
	integer :: i
	
	do i = 1, 999
		critical
			if (this_image() == 1) then
				caf(:) = num
			else
				if (caf(0) /= caf(98)) then
					print *, caf(1), caf(99)
					error stop 15
				end if
				
				if (caf(49) /= caf(50)) then
					print *, caf(49), caf(50)
					error stop 16
				end if
			end if
		end critical
	end do
	
end

