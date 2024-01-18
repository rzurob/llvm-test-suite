! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test array expressions when the
!*				 array is the result of POPCNT(array)
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_010f

	implicit none

	integer :: i
	integer, allocatable, dimension(:)	:: arr1, arr2
	integer, allocatable, dimension(:)	:: res1, res2, res3, res5, res6, res7, res8
	real, allocatable, dimension(:) 	:: res4
	integer, allocatable, dimension(:)	:: verify1, verify2, verify3, verify5, verify6
	integer, allocatable, dimension(:)	:: verify7, verify8
	real, allocatable, dimension(:)		:: verify4

	integer, allocatable, dimension(:,:)	:: arr1d, arr2d
	integer, allocatable, dimension(:,:)	:: res1d, res2d, res3d, res5d, res6d, res7d, res8d
	real, allocatable, dimension(:,:)	 	:: res4d
	integer, allocatable, dimension(:,:)		:: verify1d, verify2d, verify3d, verify5d, verify6d
	integer, allocatable, dimension(:,:)		:: verify7d, verify8d
	real, allocatable, dimension(:,:)			:: verify4d

	arr1=[(i,i=1,16,2)]
	arr2=[(i,i=2,17,2)]

	verify1=[2,3,4,4,4,5,6,5]
	verify2=[0,1,0,2,0,1,0,3]
	verify3=[1,2,4,3,4,6,9,4]
	verify4=[1.,2.,1.,3.,1.,1.,1.,4.]
	verify5=[4,5,5,6,5,6,6,7]
	verify6=[-2,-1,-1,0,-1,0,0,1]
	verify7=[3,6,6,9,6,9,9,12]
	verify8=[1,4,4,9,4,9,9,16]

	res1=POPCNT(arr1)+POPCNT(arr2)
	res2=POPCNT(arr1)-POPCNT(arr2)
	res3=POPCNT(arr1)*POPCNT(arr2)
	res4=POPCNT(arr1)/POPCNT(arr2)
	res5=POPCNT(arr1)+3
	res6=POPCNT(arr1)-3
	res7=POPCNT(arr1)*3
	res8=POPCNT(arr1)**2

	if (any(res1 /= verify1)) ERROR STOP 1
	if (any(res2 /= verify2)) ERROR STOP 2
	if (any(res3 /= verify3)) ERROR STOP 3
	if (any(res4 /= verify4)) ERROR STOP 4
	if (any(res5 /= verify5)) ERROR STOP 5
	if (any(res6 /= verify6)) ERROR STOP 6
	if (any(res7 /= verify7)) ERROR STOP 7
	if (any(res8 /= verify8)) ERROR STOP 8

	arr1d=reshape([(i,i=1,16,2)], [2,4])
	arr2d=reshape([(i,i=2,17,2)], [2,4])

	verify1d=reshape([2,3,4,4,4,5,6,5], [2,4])
	verify2d=reshape([0,1,0,2,0,1,0,3], [2,4])
	verify3d=reshape([1,2,4,3,4,6,9,4], [2,4])
	verify4d=reshape([1.,2.,1.,3.,1.,1.,1.,4.], [2,4])
	verify5d=reshape([4,5,5,6,5,6,6,7], [2,4])
	verify6d=reshape([-2,-1,-1,0,-1,0,0,1], [2,4])
	verify7d=reshape([3,6,6,9,6,9,9,12], [2,4])
	verify8d=reshape([1,4,4,9,4,9,9,16], [2,4])

	res1d=POPCNT(arr1d)+POPCNT(arr2d)
	res2d=POPCNT(arr1d)-POPCNT(arr2d)
	res3d=POPCNT(arr1d)*POPCNT(arr2d)
	res4d=POPCNT(arr1d)/POPCNT(arr2d)
	res5d=POPCNT(arr1d)+3
	res6d=POPCNT(arr1d)-3
	res7d=POPCNT(arr1d)*3
	res8d=POPCNT(arr1d)**2

	if (any(res1d /= verify1d)) ERROR STOP 9
	if (any(res2d /= verify2d)) ERROR STOP 10
	if (any(res3d /= verify3d)) ERROR STOP 11
	if (any(res4d /= verify4d)) ERROR STOP 12
	if (any(res5d /= verify5d)) ERROR STOP 13
	if (any(res6d /= verify6d)) ERROR STOP 14
	if (any(res7d /= verify7d)) ERROR STOP 15
	if (any(res8d /= verify8d)) ERROR STOP 16

end program POPCNT_010f
