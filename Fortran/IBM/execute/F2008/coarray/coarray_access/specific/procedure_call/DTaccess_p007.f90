!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p007.f
!*
!*  DATE                       : May 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray to an external procedure with same corank different cobounds.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	interface
		subroutine twaddle1(caf, i1, r1)
			type A
				sequence
				integer(8) :: i
				real(8) :: r
			end type

			type (A), intent(inout) :: caf[10:*]
			integer(8) :: i1
			real(8) :: r1
		end subroutine

		subroutine twaddle2(caf, i1, r1)
			type A
				sequence
				integer(8) :: i
				real(8) :: r
			end type

			type (A), intent(inout) :: caf(2,2)[-1:0,0:1,1:*]
			integer(8) :: i1
			real(8) :: r1
		end subroutine
	end interface

	type A
		sequence
		integer(8) :: i
		real(8) :: r
	end type

	logical :: precision_r8
	integer :: i, j

	type (A), save :: cafA[*]
	type (A) :: dtA
	type (A), save :: cafB(2,2)[2,2,*]
	type (A) :: dtB(2,2)

	cafA = A(1, 0.0)
	dtA = A(20, 55.5_8)


	call twaddle1(cafA, 10_8, 25.25_8)
	call twaddle1(cafA, 2_8, 30.25_8)
	if (cafA%i /= dtA%i) then
		print *, cafA%i
		print *, dtA%i
		error stop 20
	end if
	if (.not. precision_r8(cafA%r, dtA%r)) then
		print *, cafA%r
		print *, dtA%r
		error stop 21
	end if


	cafB%i = 1
	cafB%r = 2.0_8
	dtB%i = 29
	dtB%r = 4.0_8

	call twaddle2(cafB, 29_8, 2.0_8)
	if ( any(cafB%i /= dtB%i) ) then
		print *, "actual", cafB%i
		print *, "expected", dtB%i
		error stop 22
	end if

	do i = 1, 2
		do j = 1, 2
			if (.not. precision_r8(cafB(i,j)%r, dtB(i,j)%r)) then
				print *, "actual", cafB%r
				print *, "expected", dtB%r
				print *, i, j
				error stop 23
			end if
		end do
	end do

end


subroutine twaddle1(caf, i1, r1)
	type A
		sequence
		integer(8) :: i
		real(8) :: r
	end type

	type (A), intent(inout) :: caf[10:*]
	integer(8) :: i1
	real(8) :: r1

	caf%i = caf%i * i1
	caf%r = caf%r + r1
end subroutine


subroutine twaddle2(caf, i1, r1)
	type A
		sequence
		integer(8) :: i
		real(8) :: r
	end type

	type (A), intent(inout) :: caf(2,2)[-1:0,0:1,1:*]
	integer(8) :: i1
	real(8) :: r1

	caf%i = i1

	do i = 1, 2
		do j = 1, 2
			caf(i,j)%r = caf(i,j)%r + r1
		end do
	end do
end subroutine

