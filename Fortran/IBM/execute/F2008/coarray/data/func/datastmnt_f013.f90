!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : November 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test simple initialization with DATA for array coarrays via implied-do loops.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf1(2,2,2)[*], caf2(3,3)[2,2,*]
	real, save :: caf3(8)[1,2,3,1:*]
	integer :: i, j, k

	data (((caf1(i,j,k), i=1,2), j=1,2), k=1,2) /2,3,5,7,11,13,17,19/
	data ((caf2(i,j), i=1,3), j=1,3) /0,1,1,2,3,5,8,13,21/
	data (caf3(i), i=1,7,2) /1.,3.,5.,7./, (caf3(i), i=2,8,2) /20.,40.,60.,80./


	if ( any( reshape(caf1, (/8/)) .ne. [2, 3, 5, 7, 11, 13, 17, 19]) ) then
		print *, reshape(caf1, (/8/))
		error stop 21
	end if

	if ( any(reshape(caf2, (/9/)) .ne. [0, 1, 1, 2, 3, 5, 8, 13, 21]) ) then
		print *, reshape(caf2, (/9/))
		error stop 22
	end if

	if ( any(caf3 .ne. [1.0, 20.0, 3.0, 40.0, 5.0, 60.0, 7.0, 80.0]) ) then
		print *, caf3
		error stop 23
	end if

end
