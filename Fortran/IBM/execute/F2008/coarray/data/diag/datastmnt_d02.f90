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
!*  DESCRIPTION                : Test simple initialization with DATA for integer coarray scalars.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf0[*], caf2(3)[2,2,*]
	integer, save :: caf1(2)[1:*], caf3(4)[*]

	data caf0/95,100/
	data caf1/3*3/
	data caf2/5/
	data caf3/2*10/
end
