! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Simple test that argument in POPCNT
!*				 can be an explicit shape array,
!*				 allocatable array
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_006f

	implicit none

	integer, dimension(6) :: arr
	integer, allocatable, dimension(:) :: arr0, arr1
	integer :: res1
	integer :: res2
	integer :: res3
	integer :: res4
	integer :: res5, res6
	integer :: s

	arr = POPCNT([1,2,4,8,16,32])
	s = size(arr)

	arr0=[1,2,4,8,16,32]
	arr1=POPCNT(arr0)
	res5 = size(arr1)

	arr0=[1,2,4,8,16,32,64]
	arr1=POPCNT(arr0)
	res6 = size(arr1)

	res1 = sum(arr)
	res2 = sum(POPCNT([1,2,4,8,16,32]))
	res3 = PRODUCT(arr)
	res4 = PRODUCT(POPCNT([1,2,4,8,16,32]))

	if(res1 /= s) ERROR STOP 1
	if(res2 /= s) ERROR STOP 2
	if(res3 /= 1) ERROR STOP 3
	if(res4 /= 1) ERROR STOP 4
	if(res5 == res6) ERROR STOP 5

	call sub1(POPCNT(arr))

	contains

	subroutine sub1(x)

		integer, dimension(-1:*) :: x

		if (size(POPCNT(x(-1:4))) /= 6) ERROR STOP 6

	end subroutine sub1

end program POPCNT_006f
