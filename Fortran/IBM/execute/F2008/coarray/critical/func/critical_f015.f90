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
!*  DESCRIPTION                : Test critical construct with derived type
!*                               coarray global data.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modGATX105
	type phase
		integer :: x
		real :: y
	end type

	type (phase), save :: caf(100000)[0:*]
end module


program main
	use modGATX105
	integer :: num
	integer :: res
	real, parameter :: val = 3.141596
	logical :: precision_r4

	caf = phase(0, 0)
	res = 0
	sync all

	do j = 1, 500
		if (this_image() <= 2) then
			critical
				if (this_image() == 1) then
					caf(:)%x = this_image() * num_images()
					caf(:)%y = val
				else if (this_image() == 2) then
					if ( caf(1)[1]%x /= caf(100000)[1]%x ) then
						print *, caf(1)[1]%x, caf(100000)[1]%x
						error stop 15
					end if

					if ( caf(1)[1]%y /= caf(100000)[1]%y ) then
						print *, caf(1)[1]%y, caf(100000)[1]%y
						error stop 16
					end if
				end if
			end critical
		else
			if ( caf(1)[1]%x /= caf(100000)[1]%x ) then
				res = res + 1
			end if

			if ( caf(1)%x /= caf(100000)%x ) then
				res = res + 1
			end if
		end if
	end do

	sync all
	if (this_image() > 2) then
		if (res < 0) then
			print *, res
			error stop 17
		end if
	end if

end
