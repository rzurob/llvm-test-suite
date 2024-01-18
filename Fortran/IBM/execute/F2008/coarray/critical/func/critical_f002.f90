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
!*  DESCRIPTION                : Test critical construct with global integer coarrays
!*                               in a module procedure.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module aic
	implicit none
	integer*2, save :: caf[*]
	integer, parameter :: n = 8

contains

	subroutine sub0()
		integer :: i
		integer*2, save :: arr(n)[*]

		caf = 1
		arr = 0
		sync all

		critical
			arr(caf[n])[1] = caf[n]
			caf[n] = caf[n] + 1
		end critical

		sync all

		if (this_image() == 1) then
			if ( any(arr .ne. [(i, i = 1,n)]) ) then
				print *, arr
				error stop 15
			end if
		else
			if ( any(arr .ne. [(0, i = 1,n)]) ) then
				print *, arr
				print *, "image", this_image()
				error stop 16
			end if
		end if
	end subroutine
end module


program main
	use aic

	call sub0()

	if (caf[n] /= (n + 1)) then
		print *, caf[n]
		error stop 17
	end if
end
