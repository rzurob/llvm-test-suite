! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that the compiler will not issue error
!*				 message when the argument of POPCNT() is an array
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPCNT_008d

	implicit none

	integer, dimension(5) :: arr=[1,2,3,4,5]
	integer, dimension(5) :: res1

	res1=POPCNT(arr)
	print *, res1

end program POPCNT_008d
