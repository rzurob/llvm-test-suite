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
!*  DESCRIPTION                : Test simple initialization with DATA for character coarray scalars.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	character(1), save :: caf1[*]
	character(len=7), save :: caf2[1,2,3,4:5,6:7,8:*]
	character(len=5), save :: caf3[1,-3:-1,-10:-1,*]
	character(10), save :: caf4[0:2,0:*]

	data caf1/"coarray"/
	data caf2/"fortran"/
	data caf3/"14.1"/
	data caf4(1:5)/"abcde"/, caf4(6:10)/"vwxyz"/

	if (this_image() == 1) then
		print *, caf1
		print *, caf2
		print *, caf3
		print *, caf4
	end if

end
