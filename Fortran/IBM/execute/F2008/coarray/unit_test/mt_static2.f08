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
!*                               stored across internal units.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	implicit none
	real :: x

	if (this_image() == 1) then
		x = f1()
	else
		x = f2()
	end if

contains

	real function f1()
		real, save :: caf[*]

		print *, "begin f1:", caf
		caf[1] = this_image()
		sync all

		if (caf[1] /= this_image()) then	! should be "1"
			print *, caf[1]
			error stop 21
		end if
	end function

	real function f2()
		real, save :: caf[*]

		print *, "begin f2:", caf
		caf[1] = this_image()
		sync all

		! should be "2" to "n", where n is number of images
		if (caf[1] < 2 .or. caf[1] > num_images()) then
			print *, caf[1]
			error stop 31
		end if
	end function

end
