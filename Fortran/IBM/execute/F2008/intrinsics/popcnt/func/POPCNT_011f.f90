! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that POPCNT() can get argumen
!*				 POPCNT() or POPPAR() functions
!*				 (ex. POPCNT(POPPAR(POPCNT(arr))) )
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_011f

	implicit none
	integer :: i

	integer :: a=10, b=-15
	integer :: res1, res1a
	integer, dimension(5) :: arr1, res1b

	res1=POPCNT(POPCNT(a))
	res1a=POPCNT(POPCNT(b))
	arr1=[1,2,3,4,5]
	res1b=POPCNT(POPCNT(arr1))

	if (res1 /= 1) ERROR STOP 1
	if (res1a /= 4) ERROR STOP 2
	if (any(res1b /= [1,1,1,1,1])) ERROR STOP 3

	res1=POPCNT(POPCNT(POPPAR(a)))
	res1a=POPCNT(POPCNT(POPPAR(b)))
	arr1=[1,2,3,4,5]
	res1b=POPCNT(POPCNT(POPPAR(arr1)))

	if (res1 /= 0) ERROR STOP 4
	if (res1a /= 1) ERROR STOP 5
	if (any(res1b /= [1,1,0,1,0])) ERROR STOP 6

	res1=POPCNT(POPPAR(POPCNT(a)))
	res1a=POPCNT(POPPAR(POPCNT(b)))
	arr1=[1,2,3,4,5]
	res1b=POPCNT(POPPAR(POPCNT(arr1)))

	if (res1 /= 1) ERROR STOP 7
	if (res1a /= 0) ERROR STOP 8
	if (any(res1b /= [1,1,1,1,1])) ERROR STOP 9

end program POPCNT_011f
