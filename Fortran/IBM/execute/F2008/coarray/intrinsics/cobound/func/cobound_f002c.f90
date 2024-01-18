!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_f002c.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : September 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with coarrays
!*                               of various corank using 3 arguments. Also
!*                               test the case with 2 arguments where one
!*                               is a corray and the other is the kind.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	complex*4, save :: caf1(1)[*], caf2(1,2)[2,*], caf3(1,2,3)[0:0,0:1,0:*]
	complex*8, save :: caf4(1,2,3,4)[2,1,1,*], caf5(1,2,3,4,5)[20,0:19,-1:8,-2:7,-3:*]

	integer(1), allocatable :: arr_lo_1(:), arr_hi_1(:)
	integer(2), allocatable :: arr_lo_2(:), arr_hi_2(:)
	integer(4), allocatable :: arr_lo_4(:), arr_hi_4(:)
	integer(8), allocatable :: arr_lo_8(:), arr_hi_8(:)


!%%%%%%%%%%%%%%%%%% 3 ARGS
!##### CORANK = 1
	allocate(arr_lo_8(1), arr_hi_8(1))
	arr_lo_8(1) = lcobound(caf1, 1, 8)
	arr_hi_8(1) = ucobound(caf1, 1, 8)

	if ( any(arr_lo_8 .ne. [1_8]) ) then
		print *, arr_lo_8
		error stop 11
	end if
	if ( any(arr_hi_8 .ne. [5_8]) ) then
		print *, arr_hi_8
		error stop 12
	end if
	deallocate(arr_lo_8, arr_hi_8)
	sync all
	
!##### CORANK = 2
	allocate(arr_lo_4(2), arr_hi_4(2))
	do i = 1, 2
		arr_lo_4(i) = lcobound(caf2, i, 4)
		arr_hi_4(i) = ucobound(caf2, i, 4)
	end do
	
	if ( any(arr_lo_4 .ne. [1_4, 1_4]) ) then
		print *, arr_lo_4
		error stop 13
	end if
	if ( any(arr_hi_4 .ne. [2_4, 3_4]) ) then
		print *, arr_hi_4
		error stop 14
	end if
	deallocate(arr_lo_4, arr_hi_4)
	sync all

!##### CORANK = 3
	allocate(arr_lo_2(3), arr_hi_2(3))
	do i = 1, 3
		arr_lo_2(i) = lcobound(caf3, i, 2)
		arr_hi_2(i) = ucobound(caf3, i, 2)
	end do
	
	if ( any(arr_lo_2 .ne. [0_2, 0_2, 0_2]) ) then
		print *, arr_lo_2
		error stop 15
	end if
	if ( any(arr_hi_2 .ne. [0_2, 1_2, 2_2]) ) then
		print *, arr_hi_2
		error stop 16
	end if
	deallocate(arr_lo_2, arr_hi_2)
	sync all
	
!##### CORANK = 4
	allocate(arr_lo_1(4), arr_hi_1(4))
	do i = 1, 4
		arr_lo_1(i) = lcobound(caf4, i, 1)
		arr_hi_1(i) = ucobound(caf4, i, 1)
	end do
	
	if ( any(arr_lo_1 .ne. [1_1, 1_1, 1_1, 1_1]) ) then
		print *, arr_lo_1
		error stop 17
	end if
	if ( any(arr_hi_1 .ne. [2_1, 1_1, 1_1, 3_1]) ) then
		print *, arr_hi_1
		error stop 18
	end if
	deallocate(arr_lo_1, arr_hi_1)
	sync all
	
!##### CORANK = 5
	allocate(arr_lo_8(5), arr_hi_8(5))
	do i = 1, 5
		arr_lo_8(i) = lcobound(caf5, i, 8)
		arr_hi_8(i) = ucobound(caf5, i, 8)
	end do
	
	if ( any(arr_lo_8 .ne. [1_8, 0_8, -1_8, -2_8, -3_8]) ) then
		print *, arr_lo_8
		error stop 19
	end if
	if ( any(arr_hi_8 .ne. [20_8, 19_8, 8_8, 7_8, -3_8]) ) then
		print *, arr_hi_8
		error stop 20
	end if
	deallocate(arr_lo_8, arr_hi_8)
	sync all
	

!%%%%%%%%%%%%%%%%%% 2 ARGS
	call sub0()
	
contains

	subroutine sub0()
	
	!##### CORANK = 1
		allocate(arr_lo_1(1), arr_hi_1(1))
		arr_lo_1 = lcobound(COARRAY=caf1, KIND=1)
		arr_hi_1 = ucobound(COARRAY=caf1, KIND=1)
		
		if ( any(arr_lo_1 .ne. [1_1]) ) then
			print *, arr_lo_1
			error stop 41
		end if
		if ( any(arr_hi_1 .ne. [5_1]) ) then
			print *, arr_hi_1
			error stop 42
		end if
		deallocate(arr_lo_1, arr_hi_1)
		sync all
		
	!##### CORANK = 2
		allocate(arr_lo_2(2), arr_hi_2(2))
		arr_lo_2 = lcobound(COARRAY=caf2, KIND=2)
		arr_hi_2 = ucobound(COARRAY=caf2, KIND=2)
		
		if ( any(arr_lo_2 .ne. [1_2, 1_2]) ) then
			print *, arr_lo_2
			error stop 43
		end if
		if ( any(arr_hi_2 .ne. [2_2, 3_2]) ) then
			print *, arr_hi_2
			error stop 44
		end if
		deallocate(arr_lo_2, arr_hi_2)
		sync all
		
	!##### CORANK = 3
		allocate(arr_lo_4(3), arr_hi_4(3))
		arr_lo_4 = lcobound(COARRAY=caf3, KIND=4)
		arr_hi_4 = ucobound(COARRAY=caf3, KIND=4)
		
		if ( any(arr_lo_4 .ne. [0_4, 0_4, 0_4]) ) then
			print *, arr_lo_4
			error stop 45
		end if
		if ( any(arr_hi_4 .ne. [0_4, 1_4, 2_4]) ) then
			print *, arr_hi_4
			error stop 46
		end if
		deallocate(arr_lo_4, arr_hi_4)
		sync all
		
	!##### CORANK = 4
		allocate(arr_lo_8(4), arr_hi_8(4))
		arr_lo_8 = lcobound(COARRAY=caf4, KIND=8)
		arr_hi_8 = ucobound(COARRAY=caf4, KIND=8)
		
		if ( any(arr_lo_8 .ne. [1_8, 1_8, 1_8, 1_8]) ) then
			print *, arr_lo_8
			error stop 47
		end if
		if ( any(arr_hi_8 .ne. [2_8, 1_8, 1_8, 3_8]) ) then
			print *, arr_hi_8
			error stop 48
		end if
		deallocate(arr_lo_8, arr_hi_8)
		sync all
		
	!##### CORANK = 5
		allocate(arr_lo_4(5), arr_hi_4(5))
		arr_lo_4 = lcobound(COARRAY=caf5, KIND=4)
		arr_hi_4 = ucobound(COARRAY=caf5, KIND=4)
		
		if ( any(arr_lo_4 .ne. [1_4, 0_4, -1_4, -2_4, -3_4]) ) then
			print *, arr_lo_4
			error stop 49
		end if
		if ( any(arr_hi_4 .ne. [20_4, 19_4, 8_4, 7_4, -3_4]) ) then
			print *, arr_hi_4
			error stop 50
		end if
		deallocate(arr_lo_4, arr_hi_4)
		sync all
	end subroutine
end
