! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that invoking POPPAR() with non-constant
!*				 expression should fail as expression or
!* 				 initial value must be evaluated at compile time
!*                               and error message must be appear
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_006d

	implicit none

	integer :: var=16
	integer, parameter :: var2=16
	integer :: var1=16
	integer(POPPAR(var)) :: i=10
	integer(POPPAR(var2)) :: k=10
	integer(POPPAR(var1)) :: j=10

end program POPPAR_006d
