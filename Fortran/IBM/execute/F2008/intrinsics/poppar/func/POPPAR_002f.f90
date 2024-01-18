! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that POPPAR can be used
!* 				 as constant expression in a
!*				 specification expressioan
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPPAR_002f

	implicit none

	integer(1), parameter :: i1=1
	integer(2), parameter :: i2=10
	integer(4), parameter :: i4=100
	integer(8), parameter :: i8=1000

	integer(1), dimension(POPPAR(i1)) :: arr1
	integer(1), dimension(POPPAR(i2)+1) :: arr2
	integer(1), dimension(POPPAR(i4)) :: arr4
	integer(1), dimension(POPPAR(i8)+1) :: arr8

	if(size(arr1) /= 1) ERROR STOP 1
	if(size(arr2) /= 1) ERROR STOP 2
	if(size(arr4) /= 1) ERROR STOP 4
	if(size(arr8) /= 1) ERROR STOP 8

end program POPPAR_002f
