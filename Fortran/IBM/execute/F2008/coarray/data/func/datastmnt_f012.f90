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
!*  DESCRIPTION                : Test repeat count initialization with DATA for array coarray sections.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf1(5,1)[2,4,6,*], caf2(2,2,2)[9,9,9,9,9,9,9,9,9,*]
	real, save :: caf3(6)[1:20,0:99,1:*], caf4(2)[*]

	data caf1(1:5,1)/5*10/
	data caf2(1:2,1:2,1:2) /2*1, 6*0/
	data caf3(1:3), caf3(4:6) /3*1.0, 3*-1.0/, caf4(1:2)/2*19.75/

	if ( any( reshape(caf1,(/5/)) .ne. [10,10,10,10,10]) ) then
		print *, reshape(caf1,(/5/))
		error stop 21
	end if

	if ( any( reshape(caf2, (/8/)) .ne. [1,1,0,0,0,0,0,0]) ) then
		print *, reshape(caf2, (/8/))
		error stop 22
	end if

	if ( any(caf3 .ne. [1.0,1.0,1.0,-1.0,-1.0,-1.0]) ) then
		print *, caf3
		error stop 23
	end if

	if ( any(caf4 .ne. [19.75,19.75]) ) then
		print *, caf4
		error stop 24
	end if

end
