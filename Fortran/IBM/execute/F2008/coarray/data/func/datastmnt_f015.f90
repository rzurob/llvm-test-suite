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
!*  DESCRIPTION                : Test repeat count and simple initialization for DATA
!*                             statements on coarrays using named constants.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, parameter :: pi = 2
	real*4, parameter :: pr = 3.14
	logical, parameter :: pt = .true.

	integer, save :: caf1[2,4,6,*]
	real*4, save :: caf3(5)[1:*], caf4(2)[*]
	logical, save :: caf5[*]

	data caf1/pi/
	data caf3/5*pr/
	data caf4/pi*4.5/
	data caf5/pt/

	if (caf1 /= pi) then
		print *, caf1
		error stop 21
	end if

	if ( any(caf3 .ne. [pr,pr,pr,pr,pr]) ) then
		print *, caf3
		error stop 22
	end if

	if ( any(caf4 .ne. [4.5,4.5]) ) then
		print *, caf4
		error stop 23
	end if

	if (.not. caf5) then
		print *, caf5
		error stop 24
	end if

end
