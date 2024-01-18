! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that invoking POPCNT() with non-constant
!*				 expression should fail as expression or
!* 				 initial value must be evaluated at compile time
!*                               and error message must be appear
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPCNT_006d

	implicit none

	integer :: var=15
	integer, parameter :: var2=15
	integer :: var1=15
	integer(POPCNT(var)) :: i=1000000000
	integer(POPCNT(var2)) :: k=1000000000
	integer(POPCNT(var1)) :: j=1000000000

end program POPCNT_006d
