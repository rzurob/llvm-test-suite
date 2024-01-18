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
!*  DESCRIPTION                : Test critical construct in an external procedure
!*                               using an scalar dummy coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer(8), save :: coarray(100000)[*]
	integer(8) :: n, t

	interface
		subroutine sub(caf, num, me)
			integer(8) :: caf(100000)[*], num, me
		end subroutine
	end interface

	coarray = -1
	t = this_image()
	n = num_images()

	if (n > 2) then
		call sub(coarray, n, t)
	end if
end


subroutine sub(caf, num, me)
	integer(8) :: caf(100000)[*]
	integer(8) :: res, num, me

	res = 0
	sync all

	do n = 1, 10000
		if (this_image() <= 2) then
			critical
				if (this_image() == 1) then
					caf(:) = num
				else if (this_image() == 2) then
					if ( caf(1)[1] /= caf(100)[1] ) then
						res = res + 1
					end if
				end if
			end critical
		else
			if (this_image() > 2) then
				if ( caf(1)[1] /= caf(100)[1] ) then
					res = res + 1
				end if
			end if
		end if
	end do

	sync all
	if (this_image() <= 2) then
		if (res /= 0) then
			print *, res
			error stop 15
		end if
	else
		if (res < 0) then
			print *, res
			error stop 16
		end if
	end if
end subroutine
