!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : December 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test memory of coarray data is properly
!*                               stored across module units.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modMPC

contains

	integer function f1()
		integer, save :: caf(3)[*]

		print *, "begin f1:", caf
		caf(1)[1] = this_image()
		caf(2)[1] = this_image() * 100
		caf(3)[1] = this_image() * 1000
		sync all

		if ( any(caf[1] .ne. [1,100,1000]) ) then	! should be [1, 100, 1000]
			print *, caf[1]
			error stop 21
		end if

		f1 = 0
	end function

	subroutine sub2()
		integer, save :: caf(3)[*]

		print *, "begin sub2:", caf
		caf[1] = this_image()
		sync all

		if ( any(caf[1] .eq. [1,100,1000]) ) then	! should be [2,2,2] to [n,n,n]
			print *, caf[1]
			error stop 31
		end if
	end subroutine

end module


program main
	use modMPC
	implicit none
	integer :: x

	if (this_image() == 1) then
		x = f1()
	else
		call sub2()
	end if

end

