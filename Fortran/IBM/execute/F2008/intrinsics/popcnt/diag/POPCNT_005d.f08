! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that invoking POPCNT() with constant
!*				 expression should work as expression or
!* 				 initial value is evaluated at compile time.
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPCNT_005d

	implicit none

	integer, parameter :: const=15
	integer(POPCNT(const)) :: i=1000000000

	print *, POPCNT(i)

end program POPCNT_005d
