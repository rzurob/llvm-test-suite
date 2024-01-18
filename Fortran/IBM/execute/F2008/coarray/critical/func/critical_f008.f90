!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : January 2011
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test critical construct containing a call to
!*                               an external procedure that uses local coarrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: val

	interface
		integer function extern()
		end function
	end interface

	val = extern()
	sync all

	if (this_image() == 3) then
		if (val /= 9) then
			print *, val
			error stop 21
		end if
	else
		if (val /= 1) then
			print *, val
			error stop 22
		end if
	end if
end


integer function extern()
	character(7), save :: char(8)[0:1,2:*]		!hardcoded with 4 images, array size is double
	integer(1), save :: index[*]

	index = 1
	sync all

	critical
		char(index[3])[1,2] = "start!!"
		char(index[3] + 1)[1,2] = "finish!"

		!increment index
		index[3] = index[3] + 2
	end critical

	sync all

	if (this_image() == image_index(char, [1,2])) then
		do i = 1, size(char)
			print *, char(i)[1,2]
		end do
	end if

	extern = index
end function