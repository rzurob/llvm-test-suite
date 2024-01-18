!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with coarrays
!*                               having very large corank using 2 or 3
!*                               args to test the kind type.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	complex(8), save :: caf1(1,2,3,4,5,6,7,8,9)[1,-2:-1,1,-2:-1,1,-2:-1,1,-2:-1,1,*]
	complex(8), save :: caf2[0:0,1,1,2,3,5,8,13,21,34,34,21,13,8,5,3,2,1,1,0:*]

	integer*1 :: arr_lo1(10), arr_hi1(10)
	integer*2 :: arr_lo2(20), arr_hi2(20)
	integer*4 :: arr_lo4(10), arr_hi4(10)
	integer*8 :: arr_lo8(20), arr_hi8(20)
	integer :: size


!###### 2 ARGS
	arr_lo1 = lcobound(COARRAY=caf1, KIND=1)
	arr_hi1 = ucobound(COARRAY=caf1, KIND=1)

	if ( any(arr_lo1 .ne. [1_1,-2_1,1_1,-2_1,1_1,-2_1,1_1,-2_1,1_1,1_1]) ) then
		print *, arr_lo1
		error stop 11
	end if
	if ( any(arr_hi1 .ne. [1_1,-1_1,1_1,-1_1,1_1,-1_1,1_1,-1_1,1_1,2_1]) ) then
		print *, arr_hi1
		error stop 12
	end if
	sync all


	arr_lo2 = lcobound(COARRAY=caf2, KIND=2)
	arr_hi2 = ucobound(COARRAY=caf2, KIND=2)

	if ( any(arr_lo2 .ne. [0_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,0_2]) ) then
		print *, arr_lo2
		error stop 13
	end if
	if ( any(arr_hi2 .ne. [0_2,1_2,1_2,2_2,3_2,5_2,8_2,13_2,21_2,34_2,34_2,21_2,13_2,8_2,5_2,3_2,2_2,1_2,1_2,0_2]) ) then
		print *, arr_hi2
		error stop 14
	end if
	sync all


!###### 3 ARGS
	size = 10
	do i = 1, size
		arr_lo4(i) = lcobound(COARRAY=caf1, DIM=i, KIND=4)
		arr_hi4(i) = ucobound(COARRAY=caf1, DIM=i, KIND=4)
	end do

	if ( any(arr_lo4 .ne. [1_4,-2_4,1_4,-2_4,1_4,-2_4,1_4,-2_4,1_4,1_4]) ) then
		print *, arr_lo4
		error stop 15
	end if
	if ( any(arr_hi4 .ne. [1_4,-1_4,1_4,-1_4,1_4,-1_4,1_4,-1_4,1_4,2_4]) ) then
		print *, arr_hi4
		error stop 16
	end if
	sync all


	size = 20
	do i = 1, size
		arr_lo8(i) = lcobound(COARRAY=caf2, DIM=i, KIND=8)
		arr_hi8(i) = ucobound(COARRAY=caf2, DIM=i, KIND=8)
	end do

	if ( any(arr_lo8 .ne. [0_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,1_2,0_2]) ) then
		print *, arr_lo8
		error stop 17
	end if
	if ( any(arr_hi8 .ne. [0_2,1_2,1_2,2_2,3_2,5_2,8_2,13_2,21_2,34_2,34_2,21_2,13_2,8_2,5_2,3_2,2_2,1_2,1_2,0_2]) ) then
		print *, arr_hi8
		error stop 18
	end if
	sync all

end
