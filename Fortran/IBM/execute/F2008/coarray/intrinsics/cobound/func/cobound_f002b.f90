!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with coarrays
!*                               of various corank using 2 arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	real*8, save :: caf1[13:*], caf2[1:12,*], caf3[3,2,*], caf4[5:7,3:4,1:2,0:*]
	real*8, save :: caf5[2,2,2,2,*], caf6[4,1,1,3,1,-1:*], caf7[1:2,1,-2:-1,99:99,1,1,*]
	real*8, save :: caf8[-8:-8,-6:-6,-4:-4,-2:-2,0:0,2:2,4:6,8:*]

	integer, allocatable :: arr_lo(:), arr_hi(:)

!##### CORANK = 1
	allocate(arr_lo(1), arr_hi(1))
	arr_lo(1) = lcobound(caf1, 1)
	arr_hi(1) = ucobound(caf1, 1)
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

!##### CORANK = 2
	allocate(arr_lo(2), arr_hi(2))
	arr_lo(1) = lcobound(caf2, 1)
	arr_lo(2) = lcobound(caf2, 2)
	arr_hi(1) = ucobound(caf2, 1)
	arr_hi(2) = ucobound(caf2, 2)
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

!##### CORANK = 3
	allocate(arr_lo(3), arr_hi(3))
	do i = 1, 3
		arr_lo(i) = lcobound(caf3, i)
		arr_hi(i) = ucobound(caf3, i)
	end do
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

!##### CORANK = 4
	allocate(arr_lo(4), arr_hi(4))
	do i = 1, 4
		arr_lo(i) = lcobound(caf4, i)
		arr_hi(i) = ucobound(caf4, i)
	end do
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

!##### CORANK = 5
	allocate(arr_lo(5), arr_hi(5))
	do i = 1, 5
		arr_lo(i) = lcobound(caf5, i)
		arr_hi(i) = ucobound(caf5, i)
	end do
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

!##### CORANK = 6
	allocate(arr_lo(6), arr_hi(6))
	do i = 1, 6
		arr_lo(i) = lcobound(caf6, i)
		arr_hi(i) = ucobound(caf6, i)
	end do
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

!##### CORANK = 7
	allocate(arr_lo(7), arr_hi(7))
	do i = 1, 7
		arr_lo(i) = lcobound(caf7, i)
		arr_hi(i) = ucobound(caf7, i)
	end do
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

!##### CORANK = 8
	allocate(arr_lo(8), arr_hi(8))
	do i = 1, 8
		arr_lo(i) = lcobound(caf8, i)
		arr_hi(i) = ucobound(caf8, i)
	end do
	print *, arr_lo, ":", arr_hi
	deallocate(arr_lo, arr_hi)
	sync all

end
