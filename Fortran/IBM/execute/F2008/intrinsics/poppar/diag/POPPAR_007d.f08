! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that invoking POPCNT(TRANSFER(real, integer))
!*				should work (also will be checked character,
!*				logical instead real) (if F2008 langlvl is specified)
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_007d

	implicit none

	integer :: int=1
	real :: real=10.0

	print *, POPPAR(TRANSFER(real, int))

end program POPPAR_007d