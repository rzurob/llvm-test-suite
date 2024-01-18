! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 06, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that argument in POPPAR
!*				can be a negative integer number
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_007f

	implicit none

	integer, parameter :: int_kind2=2
	integer, parameter :: int_kind4=4
	integer, parameter :: int_kind8=8

	integer(kind=int_kind2) :: int02=-10
	integer(kind=int_kind4) :: int04=-101
	integer(kind=int_kind8) :: int08=-10

	integer :: res1, res2, res3

	res1=POPPAR(int02)
	res2=POPPAR(int04)
	res3=POPPAR(int08)

	if(res1 /= 0) ERROR STOP 1
	if(res2 /= 1) ERROR STOP 2
	if(res3 /= 0) ERROR STOP 3

end program POPPAR_007f
