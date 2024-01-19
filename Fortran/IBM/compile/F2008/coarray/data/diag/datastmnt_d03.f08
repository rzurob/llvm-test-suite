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
!*  DESCRIPTION                : DATA statement cannot initialized coindexed objects.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf0[*], caf1(2)[2,2,*]

	data caf0[1] /1/
	data caf1(1)[1,1,1] /44/, caf1(2)[1,1,1] /12/
end
