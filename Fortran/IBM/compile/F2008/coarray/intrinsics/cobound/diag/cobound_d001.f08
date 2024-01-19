!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : lcobound/ucobound require CAF support be on.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer*4, save :: caf[2,2,*], caf2[*]

	print *, lcobound(caf)
	print *, lcobound(caf2)
	print *, ucobound(caf)
	print *, ucobound(caf2)

end
