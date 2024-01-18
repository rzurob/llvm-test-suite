! *********************************************************************
!*  ===================================================================
!*  F2008 Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : POPCNT_008f
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
!*  DESCRIPTION                : Test the transformational functions 
!*				(PRODUCT(), SUM(), TRANSPOSE(matrix),
!*				 MATMUL(A,B)) where the argument(s)
!*				 is(are) POPCNT(array)
!*
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPCNT_008f
	
	implicit none
	
	integer :: i,res1, res2, res3
	integer, allocatable, dimension(:) :: arr, res4, verify4, res5, verify5
	integer, allocatable, dimension(:) :: res6, verify6
	integer, allocatable, dimension(:,:) :: arr2d, res2d, verify2d, A, B, C
	
!*********PRODUCT**************************
	
	arr=[(i,i=1,16,2)]
	arr2d=reshape([(i,i=10,20)],[2,5])
	res1=PRODUCT(POPCNT(arr))
	res2=PRODUCT(POPCNT(arr2d))
	res3=PRODUCT(POPCNT(arr2d), MASK=POPCNT(arr2d) .lt. 4)
	verify4=[6,6,12,2,6]
	res4=PRODUCT(POPCNT(arr2d), dim = 1)
	verify5=[24,216]
	res5=PRODUCT(POPCNT(arr2d), dim = 2)
	verify6=[8,2]
	res6=PRODUCT(POPCNT(arr2d), dim = 2, MASK=POPCNT(arr2d) .lt. 3)
		
    if (res1 /= 864) ERROR STOP 1
	if (res2 /= 5184) ERROR STOP 2
	if (res3 /= 1296) ERROR STOP 3
	if (any(res4 /= verify4)) ERROR STOP 4
	if (any(res5 /= verify5)) ERROR STOP 5
	if (any(res6 /= verify6)) ERROR STOP 6
	
!******************************************	

!*********SUM******************************
	
	arr=[(i,i=1,16,2)]
	arr2d=reshape([(i,i=10,20)],[2,5])
	res1=SUM(POPCNT(arr))
	res2=SUM(POPCNT(arr2d))
	res3=SUM(POPCNT(arr2d), MASK=POPCNT(arr2d) .lt. 4)
	verify4=[5,5,7,3,5]
	res4=SUM(POPCNT(arr2d), dim = 1)
	verify5=[10,15]
	res5=SUM(POPCNT(arr2d), dim = 2)
	verify6=[7,2]
	res6=SUM(POPCNT(arr2d), dim = 2, MASK=POPCNT(arr2d) .lt. 3)
		
    if (res1 /= 20) ERROR STOP 7
	if (res2 /= 25) ERROR STOP 8
	if (res3 /= 21) ERROR STOP 9
	if (any(res4 /= verify4)) ERROR STOP 10
	if (any(res5 /= verify5)) ERROR STOP 11
	if (any(res6 /= verify6)) ERROR STOP 12
	
!******************************************	

!*********TRANSPOSE************************
	
	arr2d=reshape([(i,i=10,20)],[2,5])
	res2d=TRANSPOSE(POPCNT(arr2d))
	verify2d=reshape([2,2,3,1,2,3,3,4,2,3], [5,2])
		
    if (any(res2d /= verify2d)) ERROR STOP 13
		
!******************************************	

!*********MATMUL************************
	
	A=reshape([(i,i=10,20)],[2,5])
	B=reshape([(i,i=10,20)],[5,2])
	C=MATMUL(POPCNT(A),POPCNT(B))
	verify2d=reshape([25,38,24,36], [2,2])
		
    if (any(C /= verify2d)) ERROR STOP 14
		
!******************************************

	
	
end program POPCNT_008f
