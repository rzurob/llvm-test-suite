!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : May 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray to an module procedure with different corank.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modTRON

	type A
		integer(2) :: i
		real(8) :: r
	end type

contains

	subroutine twaddle1(caf, i1, r1)
		type (A), intent(inout) :: caf[0:*]
		integer(2) :: i1
		real(8) :: r1

		caf%i = caf%i * i1
		caf%r = caf%r - r1
	end subroutine


	subroutine twaddle2(caf, i1, r1)
		type (A), intent(inout) :: caf(3,3)[0:2,0:3,0:4,*]
		integer(2) :: i1
		real(8) :: r1

		caf%i = i1

		do i = 1, ubound(caf,1)
			do j = 1, ubound(caf,2)
				caf(i,j)%r = caf(i,j)%r + r1
			end do
		end do
	end subroutine

end module


program main

	use modTRON
	implicit none

	logical :: precision_r8
	integer :: i, j

	type (A), save :: cafA[1,2,*]
	type (A) :: dtA
	type (A), save :: cafB(3,3)[*]
	type (A) :: dtB(3,3)

	cafA = A(1_2, 0.0_8)
	dtA = A(2000_2, -22.893_8)


	call twaddle1(cafA, 500_2, 10.1346_8)
	call twaddle1(cafA, 4_2, 12.7584_8)
	if (cafA%i /= dtA%i) then
		print *, "actual", cafA%i
		print *, "expected", dtA%i
		error stop 20
	end if
	if (.not. precision_r8(cafA%r, dtA%r)) then
		print *, "actual", cafA%r
		print *, "expected", dtA%r
		error stop 21
	end if


	cafB%i = 99_2
	cafB%r = 2.636_8
	dtB%i = 128_2
	dtB%r = 7.734762_8

	call twaddle2(cafB, 128_2, 5.098762_8)
	if ( any(cafB%i /= dtB%i) ) then
		print *, "actual", cafB%i
		print *, "expected", dtB%i
		error stop 22
	end if

	do i = lbound(cafB,1), ubound(cafB,1)
		do j = lbound(cafB,2), ubound(cafB,2)
			if (.not. precision_r8(cafB(i,j)%r, dtB(i,j)%r)) then
				print *, "actual", cafB%r
				print *, "expected", dtB%r
				print *, i, j
				error stop 23
			end if
		end do
	end do

end

