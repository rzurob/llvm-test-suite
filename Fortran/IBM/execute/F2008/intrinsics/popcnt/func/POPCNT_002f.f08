! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 16, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that POPCNT can be used
!* 				 as constant expression in a
!*				 specification expressioan
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPCNT_002f

	implicit none

	integer(1), parameter :: i1=1
	integer(2), parameter :: i2=10
	integer(4), parameter :: i4=100
	integer(8), parameter :: i8=1000

	integer(1), dimension(POPCNT(i1)) :: arr1
	integer(1), dimension(POPCNT(i2)) :: arr2
	integer(1), dimension(POPCNT(i4)) :: arr4
	integer(1), dimension(POPCNT(i8)) :: arr8

	if(size(arr1) /= 1) ERROR STOP 1
	if(size(arr2) /= 2) ERROR STOP 2
	if(size(arr4) /= 3) ERROR STOP 4
	if(size(arr8) /= 6) ERROR STOP 8

end program POPCNT_002f
