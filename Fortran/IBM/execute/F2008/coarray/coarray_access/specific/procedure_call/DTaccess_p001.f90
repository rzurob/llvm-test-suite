!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p001.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : May 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray to a procedure which examines it and modifies it.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	type A
		integer :: i
		real(4) :: r
	end type

	logical :: precision_r4
	integer :: i, j
	
	type (A), save :: cafA[*]
	type (A) :: dtA
	type (A), save :: cafB(2,2)[*]
	type (A) :: dtB(2,2)

	cafA = A(1, 0.0)
	dtA = A(10, 100.0_4)

	call twaddle1(cafA, 2, 80.25_4)
	call twaddle1(cafA, 5, 19.75_4)
	if (.not. compare(cafA,dtA)) then
		print *, cafA
		print *, dtA
		error stop 21
	end if
	
	
	cafB%i = 1
	cafB%r = 1.0
	dtB%i = 13
	dtB%r = reshape([2.0, 2.0, 2.0, 2.0], [2,2])
	
	call twaddle2(cafB, 13, 1.0)
	if ( any(cafB%i /= dtB%i) ) then
		print *, "actual", cafB%i
		print *, "expected", dtB%i
		error stop 22
	end if
	
	do i = 1, 2
		do j = 1, 2
			if (.not. precision_r4(cafB(i,j)%r, dtB(i,j)%r)) then
				print *, "actual", cafB%r
				print *, "expected", dtB%r
				print *, i, j
				error stop 23
			end if
		end do
	end do
	
contains

	subroutine twaddle1(caf, i1, r1)
		type (A), intent(inout) :: caf[0:*]
		integer :: i1
		real(4) :: r1
		
		caf%i = caf%i * i1
		caf%r = caf%r + r1
	end subroutine

	subroutine twaddle2(caf, i1, r1)
		type (A), intent(inout) :: caf(2,2)[*]
		integer :: i1
		real(4) :: r1
		
		caf%i = i1
		
		do i = 1, 2
			do j = 1, 2
				caf(i,j)%r = caf(i,j)%r + r1
			end do
		end do
	end subroutine
	
	logical function compare(caf, dt)
		type (A) :: caf[*], dt
		compare = .true.
		
		if (caf%i /= dt%i) compare = .false.
		if (.not. precision_r4(caf%r, dt%r)) compare = .false.
	end function

end
