! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 07, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test the transformational functions
!*				(PRODUCT(), SUM(), TRANSPOSE(matrix),
!*				 MATMUL(A,B)) where the argument(s)
!*				 is(are) POPPAR(array)
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_008f

	implicit none

	integer :: i,res1, res2, res3
	integer, allocatable, dimension(:) :: arr, res4, verify4, res5, verify5
	integer, allocatable, dimension(:) :: res6, verify6
	integer, allocatable, dimension(:,:) :: arr2d, res2d, verify2d, A, B, C

!*********PRODUCT**************************

	arr=[(i,i=1,16,2)]
	arr2d=reshape([(i,i=10,20)],[2,5])
	res1=PRODUCT(POPPAR(arr))
	res2=PRODUCT(POPPAR(arr2d))
	res3=PRODUCT(POPPAR(arr2d), MASK=POPPAR(arr2d) .ne. 0)
	verify4=[0,0,0,0,0]
	res4=PRODUCT(POPPAR(arr2d), dim = 1)
	verify5=[0,0]
	res5=PRODUCT(POPPAR(arr2d), dim = 2)
	verify6=[1,1]
	res6=PRODUCT(POPPAR(arr2d), dim = 2, MASK=POPPAR(arr2d) .ne. 0)

	if (res1 /= 0) ERROR STOP 1
	if (res2 /= 0) ERROR STOP 2
	if (res3 /= 1) ERROR STOP 3
	if (any(res4 /= verify4)) ERROR STOP 4
	if (any(res5 /= verify5)) ERROR STOP 5
	if (any(res6 /= verify6)) ERROR STOP 6

!******************************************

!*********SUM******************************

	arr=[(i,i=1,16,2)]
	arr2d=reshape([(i,i=10,20)],[2,5])
	res1=SUM(POPPAR(arr))
	res2=SUM(POPPAR(arr2d))
	res3=SUM(POPPAR(arr2d), MASK=POPPAR(arr2d) .ne. 1)
	verify4=[1,1,1,1,1]
	res4=SUM(POPPAR(arr2d), dim = 1)
	verify5=[2,3]
	res5=SUM(POPPAR(arr2d), dim = 2)
	verify6=[0,0]
	res6=SUM(POPPAR(arr2d), dim = 2, MASK=POPPAR(arr2d) .ne. 1)

    if (res1 /= 4) ERROR STOP 7
	if (res2 /= 5) ERROR STOP 8
	if (res3 /= 0) ERROR STOP 9
	if (any(res4 /= verify4)) ERROR STOP 10
	if (any(res5 /= verify5)) ERROR STOP 11
	if (any(res6 /= verify6)) ERROR STOP 12

!******************************************

!*********TRANSPOSE************************

	arr2d=reshape([(i,i=10,20)],[2,5])
	res2d=TRANSPOSE(POPPAR(arr2d))
	verify2d=reshape([0,0,1,1,0,1,1,0,0,1], [5,2])

    if (any(res2d /= verify2d)) ERROR STOP 13

!******************************************

!*********MATMUL************************

	A=reshape([(i,i=10,20)],[2,5])
	B=reshape([(i,i=10,20)],[5,2])
	C=MATMUL(POPPAR(A),POPPAR(B))
	verify2d=reshape([1,2,0,2], [2,2])

    if (any(C /= verify2d)) ERROR STOP 14

!******************************************

end program POPPAR_008f
