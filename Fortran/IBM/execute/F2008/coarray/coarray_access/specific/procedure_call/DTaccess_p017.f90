!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p017.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : June 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray with an allocatable component which examines and modifies it.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none
	integer :: i, j, k
	
	type coarray_cluster
		integer(1), allocatable :: i1
		integer(2), allocatable :: i2(:)
		integer(4), allocatable :: i4(:,:)
	end type
	type(coarray_cluster), save, codimension[*] :: CAF
	
	integer(1), parameter :: mid1 = 1_1
	integer(2), parameter :: mid2 = 499_2
	integer(4), parameter :: mid4 = 10807551_4
	
	
	allocate(CAF%i1, source = 25_1)
	call subZ(CAF%i1)
	CAF%i1 = CAF%i1 + 1
	if (CAF%i1 /= 52_1) then
		print *, "actual", CAF%i1
		print *, "expected", 52
		error stop 14
	end if
	
	allocate(CAF%i2(10))
	CAF%i2 = [1,2,3,4,5,6,7,8,9,10]
	call sub0(CAF%i2)
	deallocate(CAF%i2)
	allocate(CAF%i2(20))
	CAF%i2 = (/ (i, i = 1,20) /)
	call sub0(CAF%i2)
	
	do k = 1, 10
		call sub1(CAF%i4)
		if ( any(reshape(CAF%i4, [4]) .ne. (/2,3,3,5/)) ) then
			print *, "actual", CAF%i4
			print *, "expected", (/2,3,3,5/)
			print *, k
			error stop 16
		end if
		deallocate(CAF%i4)
	end do
	
	
	deallocate(CAF%i1)
	deallocate(CAF%i2)

contains

	subroutine subZ(var)
		integer(1), allocatable, intent(inout) :: var
		
		var = var * 2
		var = var + 1
	end subroutine

	subroutine sub0(arr)
		integer(2), allocatable, intent(inout) :: arr(:)
		
		do i = 1, size(arr)
			if (i /= arr(i)) then
				print *, "expected", i
				print *, "actual", arr(i)
				error stop 15
			end if
		end do
	end subroutine
	
	
	subroutine sub1(arr)
		integer(4), allocatable, intent(inout) :: arr(:,:)
		
		allocate(arr(2,2))
		arr = reshape([1,1,1,1], (/2,2/))
		
		do i = 1, ubound(arr, 1)
			do j = 1, ubound(arr, 2)
				arr(i,j) = arr(i,j) + (i * j)
			end do
		end do
	end subroutine
	
end
