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
!*  DESCRIPTION                : Test critical construct with global real coarrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module muse

	implicit none
	real*8, parameter :: r = -9.25
	real*8, save :: caf(1001)[*]

end module


program main

	use muse

	do i = 1, 1000
		critical
			if (this_image() == 1) then
				caf(:) = r
			else
				if ( caf(1) /= caf(100) ) then
					print *, caf(1), caf(100)
					error stop 15
				end if
			end if
		end critical
	end do

end

