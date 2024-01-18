! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPPAR_010f
!*
!*  PROGRAMMER                 : Denis Navotniy
!*  DATE                       : December 07, 2010
!*
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test array expressions when the
!*				 array is the result of POPPAR(array)
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_010f
	
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
	integer, allocatable, dimension(:,:)	:: verify1d, verify2d, verify3d, verify5d, verify6d
	integer, allocatable, dimension(:,:)	:: verify7d, verify8d
	real, allocatable, dimension(:,:)		:: verify4d
	
	arr1=[(i,i=1,16,2)]
	arr2=[(i,i=2,17,2)]
	
	verify1=[2,1,0,2,0,1,2,1]
	verify2=[0,-1,0,0,0,1,0,-1]
	verify3=[1,0,0,1,0,0,1,0]
	!verify4=[1.,2.,1.,3.,1.,1.,1.,4.]
	verify5=[4,3,3,4,3,4,4,3]
	verify6=[-2,-3,-3,-2,-3,-2,-2,-3]
	verify7=[3,0,0,3,0,3,3,0]
	verify8=[1,0,0,1,0,1,1,0]
	
	res1=POPPAR(arr1)+POPPAR(arr2)
	res2=POPPAR(arr1)-POPPAR(arr2)
	res3=POPPAR(arr1)*POPPAR(arr2)
	!res4=POPPAR(arr1)/POPPAR(arr2)
	res5=POPPAR(arr1)+3
	res6=POPPAR(arr1)-3
	res7=POPPAR(arr1)*3
	res8=POPPAR(arr1)**2
	
	if (any(res1 /= verify1)) ERROR STOP 1
	if (any(res2 /= verify2)) ERROR STOP 2
	if (any(res3 /= verify3)) ERROR STOP 3
	!if (any(res4 /= verify4)) ERROR STOP 4
	if (any(res5 /= verify5)) ERROR STOP 5
	if (any(res6 /= verify6)) ERROR STOP 6
	if (any(res7 /= verify7)) ERROR STOP 7
	if (any(res8 /= verify8)) ERROR STOP 8
	
	arr1d=reshape([(i,i=1,16,2)], [2,4])
	arr2d=reshape([(i,i=2,17,2)], [2,4])
	
	verify1d=reshape([2,1,0,2,0,1,2,1], [2,4])
	verify2d=reshape([0,-1,0,0,0,1,0,-1], [2,4])
	verify3d=reshape([1,0,0,1,0,0,1,0], [2,4])
	!verify4d=reshape([1.,2.,1.,3.,1.,1.,1.,4.], [2,4])
	verify5d=reshape([4,3,3,4,3,4,4,3], [2,4])
	verify6d=reshape([-2,-3,-3,-2,-3,-2,-2,-3], [2,4])
	verify7d=reshape([3,0,0,3,0,3,3,0], [2,4])
	verify8d=reshape([1,0,0,1,0,1,1,0], [2,4])
	
	res1d=POPPAR(arr1d)+POPPAR(arr2d)
	res2d=POPPAR(arr1d)-POPPAR(arr2d)
	res3d=POPPAR(arr1d)*POPPAR(arr2d)
	!res4d=POPPAR(arr1d)/POPPAR(arr2d)
	res5d=POPPAR(arr1d)+3
	res6d=POPPAR(arr1d)-3
	res7d=POPPAR(arr1d)*3
	res8d=POPPAR(arr1d)**2
	
	if (any(res1d /= verify1d)) ERROR STOP 9
	if (any(res2d /= verify2d)) ERROR STOP 10
	if (any(res3d /= verify3d)) ERROR STOP 11
	!if (any(res4d /= verify4d)) ERROR STOP 12
	if (any(res5d /= verify5d)) ERROR STOP 13
	if (any(res6d /= verify6d)) ERROR STOP 14
	if (any(res7d /= verify7d)) ERROR STOP 15
	if (any(res8d /= verify8d)) ERROR STOP 16
	
end program POPPAR_010f
