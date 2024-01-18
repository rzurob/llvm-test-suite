! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that the compiler will not issue error
!*				 message when the argument of POPPAR() is an array
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_008d

	implicit none

	integer, dimension(5) :: arr=[1,2,3,4,5]
	integer, dimension(5) :: res1

	res1=POPPAR(arr)
	print *, res1

end program POPPAR_008d
