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
!*  DESCRIPTION                : Test simple initialization with DATA for array coarray elements.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf1(5)[1,2,*], caf2(2,2,2)[*]
	real, save :: caf3(2)[1,1,1,1:*], caf4(2)[3:*]

	data caf1(1)/-1/, caf1(2)/-2/, caf1(3)/-3/, caf1(4)/-4/, caf1(5)/-5/
	data caf2(1,1,1), caf2(1,1,2), caf2(1,2,1), caf2(1,2,2), &
		caf2(2,1,1), caf2(2,1,2), caf2(2,2,1), caf2(2,2,2) /1,2,3,4,5,6,7,8/
	data caf3(1),caf3(2),caf4(1),caf4(2)/10.,20.,30.0,40.25/

	call output()

contains

	subroutine output()
		print *, caf1
		sync all
		print *, caf2
		sync all
		print *, caf3
		sync all
		print *, caf4
	end subroutine

end
