! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that if F2008 langlvl is specified,
!*				 the compiler will not issue error message
!*				 when the  POPPAR() is constant expression
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_004d

	implicit none

	integer, parameter :: i = POPPAR(100)
	integer, dimension(POPPAR(100)) :: arr
	integer :: res1
	integer :: res2

	if(size(arr) /= 1) ERROR STOP 1
	if(i /= 1) ERROR STOP 2

end program POPPAR_004d
