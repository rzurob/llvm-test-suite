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
!*  DESCRIPTION                : Test reat count initialization with DATA for coarray scalars.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf1[*], caf2[1,2,3,-1:5,6:7,8:*], caf3[2,2,1:*]
	real, save :: caf4[*], caf5[*], caf6[*], caf7[*], caf8[*]
	real*8, save :: caf9[*]

	data caf1,caf2,caf3/3*-1/
	data caf4,caf5,caf6,caf7,caf8/5*5.5/
	data caf9/1*-2./

	print *, caf1, ":", caf2, ":", caf3
	sync all

	print *, caf4, ":", caf5, ":", caf6, ":", caf7, ":", caf8
	sync all

	print *, caf9

end
