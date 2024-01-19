! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 06, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that array argument in POPPAR() may
!*				 have different dimension or size, and also
!*				 assumed shape/size array dummy argument as
!*				 argument of these intrinsics
!*
!*  TARGET(S)                  :
!* ===================================================================
program POPPAR_012f

	implicit none

	integer :: i,x,y,z,t,N1,N2,N3,N4
	integer ::	size1, size2

	integer, dimension(1) :: arr1, res1
	integer, dimension(5) :: arr2, res2
  	integer, allocatable, dimension(:) :: arr3, res3
	integer, dimension(-1:3) :: arr4, res4

	integer, dimension(3,4) :: arr5, res5, verify5
	integer, allocatable, dimension(:,:) :: arr6, res6
	integer, dimension(-10:-8,8:9) :: arr7, res7, verify7

	integer, dimension(2,3,4,2) :: arr8, res8, verify8
	integer, allocatable, dimension(:,:,:,:) :: arr9, res9
	integer, dimension(1:2,-2:-1,10:11,-1:1) :: arr10, res10, verify10

!******** 1 dimension ******************************************

	arr1=(/12/)
	res1=POPPAR(arr1)

	arr2=[1,2,3,4,5]
	res2=POPPAR(arr2)

	arr3=[1,2,3,4,5]
	res3=POPPAR(arr3)
	size1=size(res3)

	arr3=[1,2,3,4,5,6,7,8,9,10]
	res3=POPPAR(arr3)
	size2=size(res3)

	arr4=[1,2,3,4,5]
	res4=POPPAR([arr4(-1), arr4(0), arr4(1), arr4(2), arr4(3)])

	if (any(res1 /= 0)) ERROR STOP 1
	if (any(res2 /= [1,1,0,1,0])) ERROR STOP 2
	if (size1 == size2) ERROR STOP 3
	if (any(res4 /= [1,1,0,1,0])) ERROR STOP 4

!****************************************************************

!******** 2 dimensions ******************************************

	verify5=reshape([1,1,0,1,0,0,1,1,0,0,1,0], [3,4])
	arr5=reshape([(i,i=1,12)], [3,4])
	res5=POPPAR(arr5)

	arr6=reshape([(i,i=1,12)], [3,4])
	res6=POPPAR(arr6)
	size1=size(res6)

	arr6=reshape([(i,i=1,15)], [3,5])
	res6=POPPAR(arr6)
	size2=size(res6)

	verify7=reshape([1,1,0,1,0,0], [3,2])
	arr7=reshape([(i,i=1,6)], [3,2])
	res7=reshape(POPPAR([ arr7(-10,8), arr7(-9,8), arr7(-8,8), arr7(-10,9), &
	arr7(-9,9), arr7(-8,9)]), [3,2])

	if (any(res5 /= verify5)) ERROR STOP 5
	if (size1 == size2) ERROR STOP 6
	if (any(res7 /= verify7)) ERROR STOP 7

!****************************************************************

!******** 4 dimensions ******************************************

	do x=1,2
		do y=1,3
			do z=1,4
				do t=1,2
					arr8(x,y,z,t) = x*y*z*t
					res8(x,y,z,t) = POPPAR(arr8(x,y,z,t))
					verify8(x,y,z,t) = POPPAR(x*y*z*t)
				end do
			end do
		end do
	end do

	N1=1
	N2=2
	N3=3
	N4=4

	allocate(arr9(N1,N2,N3,N4), res9(N1,N2,N3,N4))
	size1=sizeofarray(N1,N2,N3,N4)
	deallocate(arr9, res9)

	N1=2
	N2=3
	N3=4
	N4=5

	allocate(arr9(N1,N2,N3,N4), res9(N1,N2,N3,N4))
	size2=sizeofarray(N1,N2,N3,N4)
	deallocate(arr9, res9)

	do x=1,2
		do y=-2,-1
			do z=10,11
				do t=-1,1
					arr10(x,y,z,t) = x*y*z*t
					res10(x,y,z,t) = POPPAR(arr10(x,y,z,t))
					verify10(x,y,z,t) = POPPAR(x*y*z*t)
				end do
			end do
		end do
	end do

	if (any(res8 /= verify8)) ERROR STOP 8
	if (size1 == size2) ERROR STOP 9
	if (res10(1,-2,10,-1) /= verify10(1,-2,10,-1)) ERROR STOP 10
	if (res10(2,-1,11,1) /= verify10(2,-1,11,1)) ERROR STOP 11

!****************************************************************

!*************** Dummy arrays ***********************************

	call sub1(POPPAR(arr2), POPPAR(arr6), POPPAR(arr8), 5)

!****************************************************************

!*************** Assumed size ***********************************

	call sub2(POPPAR(arr4), POPPAR(arr6), POPPAR(arr8))

!****************************************************************

	contains

	integer function sizeofarray(x1,x2,x3,x4)

		integer :: x1, x2, x3, x4

		do x=1,N1
			do y=1,N2
				do z=1,N3
					do t=1,N4
						arr9(x,y,z,t) = x*y*z*t
						res9(x,y,z,t) = POPPAR(arr9(x,y,z,t))
					end do
				end do
			end do
	    end do

		sizeofarray=size(res9)

	end function sizeofarray

	subroutine sub1(x,y,z,dim1)

		integer :: dim1
		integer, dimension(dim1) :: t
		integer, dimension(:) :: x
		integer, dimension(:,:) :: y
		integer, dimension(:,:,:,:) :: z

		if (size(POPPAR(x)) /= 5) ERROR STOP 12
		if (size(POPPAR(y)) /= 15) ERROR STOP 13
		if (size(POPPAR(z)) /= 48) ERROR STOP 14

	end subroutine sub1

	subroutine sub2(x,y,z)

		integer, dimension(-1:*) :: x
		integer, dimension(1:3,-1:*) :: y
		integer, dimension(-1:0,1:3,-2:1,-5:*) :: z

		if (size(POPPAR(x(-1:3))) /= 5) ERROR STOP 15
		if (size(POPPAR(y(1:3,-1:3))) /= 15) ERROR STOP 16
		if (size(POPPAR(z(-1:0,1:3,-2:1,-5:-4))) /= 48) ERROR STOP 17

	end subroutine sub2

end program POPPAR_012f

