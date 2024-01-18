! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 06, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that argument in POPCNT can
!*				 be a negative integer number
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_007f

	implicit none

	integer, parameter :: int_kind2=2
	integer, parameter :: int_kind4=4
	integer, parameter :: int_kind8=8

	integer(kind=int_kind2) :: int02=-10
	integer(kind=int_kind4) :: int04=-10
	integer(kind=int_kind8) :: int08=-10

	integer :: res1, res2, res3
	integer :: verify1, verify2, verify3

	integer :: i

	res1=POPCNT(int02)
	res2=POPCNT(int04)
	res3=POPCNT(int08)

	verify1=8*int_kind2-POPCNT(abs(int02))
	verify2=8*int_kind4-POPCNT(abs(int04))
	verify3=8*int_kind8-POPCNT(abs(int08))

	if(res1 /= verify1) ERROR STOP 1
	if(res2 /= verify2) ERROR STOP 2
	if(res3 /= verify3) ERROR STOP 3

end program POPCNT_007f
