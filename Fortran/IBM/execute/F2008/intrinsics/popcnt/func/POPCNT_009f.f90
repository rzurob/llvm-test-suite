! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 06, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test the transformational functions
!*				(RESHAPE()) where the argument is POPCNT(array)
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_009f

	implicit none
	integer :: i

	integer, dimension(3,4) :: res1, res1a, res1b, verify1
	integer, dimension(12) :: arr1

	integer, dimension(3,4) :: res2, res2a, res2b, verify2
	integer, dimension(6) :: arr2

	arr1	=	[1,2,3,4,5,6,7,8,9,10,11,12]
	verify1	=	reshape(source=[1,1,2,1,2,2,3,1,2,2,3,2], shape=[3,4])
	res1	=	reshape(source=POPCNT([1,2,3,4,5,6,7,8,9,10,11,12]), shape=[3,4])
	res1a	=	reshape(source=POPCNT(arr1), shape=[3,4])
	res1b	=	reshape(POPCNT(arr1), [3,4])

	if (any(res1 /= verify1)) ERROR STOP 1
	if (any(res1a /= verify1)) ERROR STOP 2
	if (any(res1b /= verify1)) ERROR STOP 3

	arr2	=	[1,2,3,4,5,6]
	verify2	=	reshape(source=[1,1,2,1,2,2], shape=[3,4], pad=[10,20], order=[2,1])
	res2	=	reshape(source=POPCNT([1,2,3,4,5,6]), shape=[3,4], pad=[10,20], order=[2,1])
	res2a	=	reshape(source=POPCNT(arr2), shape=[3,4], pad=[10,20], order=[2,1])
	res2b	=	reshape(POPCNT(arr2), [3,4], [10,20], [2,1])

	if (any(res2 /= verify2)) ERROR STOP 4
	if (any(res2a /= verify2)) ERROR STOP 5
	if (any(res2b /= verify2)) ERROR STOP 6

end program POPCNT_009f
