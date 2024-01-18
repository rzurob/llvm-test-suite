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
!*  DESCRIPTION                : Test lcobound/ucobound with 2/3 arguments
!*                               with a dummy argument coarray, checking the kind type.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	implicit none

contains

	integer function three(coary)
		integer(2) :: coary[7:9,1:2,0:*]
		integer, parameter :: n = 3
		integer(2) :: arr1(n), arr2(n)
		integer :: i

		do i = 1, n
			arr1(i) = lcobound(coary, i, 2)
			arr2(i) = ucobound(coary, i, 2)
		end do

		if ( any(arr1 .ne. [7_2, 1_2, 0_2]) ) then
			print *, arr1
			error stop 13
		end if
		if ( any(arr2 .ne. [9_2, 2_2, 3_2]) ) then
			print *, arr2
			error stop 14
		end if
		sync all

		three =  1
	end function


	subroutine two(coary)
		real(4) :: coary[3:4,3:4,3:4,*]
		integer, parameter :: n = 4
		integer(4) :: arr_lo(n), arr_hi(n)

		arr_lo = lcobound(KIND=4, COARRAY=coary)
		arr_hi = ucobound(KIND=4, COARRAY=coary)

		if ( any(arr_lo .ne. [3_4, 3_4, 3_4, 1_4]) ) then
			print *, arr_lo
			error stop 17
		end if
		if ( any(arr_hi .ne. [4_4, 4_4, 4_4, 16_4]) ) then
			print *, arr_hi
			error stop 18
		end if
		sync all
	end subroutine

end module


program main

	use modFDC
	integer(2), save :: caf1[0:1,2,3:4,1,11:*]
	real(4), save :: caf2[-3:*]
	integer, parameter :: n = 5
	integer(2), allocatable :: arr1(:), arr2(:)
	integer(4), allocatable :: arr_lo(:), arr_hi(:)
	integer :: x


!#### Three Arguments
	allocate(arr1(n), arr2(n))
	do i = 1, n
		arr1(i) = lcobound(caf1, i, 1)
		arr2(i) = ucobound(caf1, i, 1)
	end do

	if ( any(arr1 .ne. [0_2, 1_2, 3_2, 1_2, 11_2]) ) then
		print *, arr1
		error stop 11
	end if
	if ( any(arr2 .ne. [1_2, 2_2, 4_2, 1_2, 13_2]) ) then
		print *, arr2
		error stop 12
	end if
	deallocate(arr1, arr2)
	sync all

	x = three(caf1)


!#### Two Arguments
	allocate(arr_lo(1), arr_hi(1))
	arr_lo = lcobound(COARRAY=caf2, KIND=4)
	arr_hi = ucobound(COARRAY=caf2, KIND=4)

	if ( any(arr_lo .ne. [-3_4]) ) then
		print *, arr_lo
		error stop 15
	end if
	if ( any(arr_hi .ne. [20_4]) ) then
		print *, arr_hi
		error stop 16
	end if
	deallocate(arr_lo, arr_hi)
	sync all

	call two(caf2)

end
