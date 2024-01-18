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
!*  DESCRIPTION                : Verify the value of num_images() called within
!*                               a critical construct.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf(79)[*]
	integer :: bad
	bad = 0
	sync all

	do i = 1, 100000
		if (this_image() <= 2) then
			critical
				if (this_image() == 1) then
					caf(:)[2] = num_images()
				else if (this_image() == 2) then
					if ( caf(1) /= caf(79) ) then
						bad = bad + 1
					end if
				end if
			end critical
		else
			if ( caf(1)[1] /= caf(100)[1] ) then
				bad = bad + 1
			end if
		end if
	end do

	sync all
	if (this_image() <= 2) then
		if (bad /= 0) then
			print *, bad
			error stop 15
		end if
	else
		if (bad < 0) then
			print *, bad
			error stop 16
		end if
	end if


end
