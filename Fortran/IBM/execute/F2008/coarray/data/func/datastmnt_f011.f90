!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : November 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test simple initialization with DATA for array coarray sections.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf1(5)[1,2,*], caf2(2,2,2)[*]
	real, save :: caf3(2)[1,1,1,1:*], caf4(2)[3:*]

	data caf1(1:3)/1,2,3/, caf1(4:5)/8,9/
	data caf2(1,1:2,1:2), caf2(2,1:2,1), caf2(2,1:2,2) /1,2,3,4,5,6,7,8/
	data caf3(1:1), caf3(2:2), caf4(1:2) /-2.0,-4.0,-6.0,-8.0/


	if ( any(caf1 .ne. [1,2,3,8,9]) ) then
		print *, caf1
		error stop 21
	end if

	if ( any(reshape(caf2, (/8/)) .ne. [1,5,2,6,3,7,4,8]) ) then
		print *, reshape(caf2, (/8/))
		error stop 22
	end if

	if ( any(caf3 .ne. [-2.0,-4.0]) ) then
		print *, caf3
		error stop 23
	end if

	if ( any(caf4 .ne. [-6.0,-8.0]) ) then
		print *, caf4
		error stop 24
	end if

end
