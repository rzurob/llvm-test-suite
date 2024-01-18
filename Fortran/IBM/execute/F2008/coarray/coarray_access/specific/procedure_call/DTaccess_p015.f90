!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p015.f
!*
!*  DATE                       : June 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray component that is an array.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modTORT

	type black_box
		integer*8 :: i(2,2)
		real*8 :: r(4)
	end type

end module


program main

	use modTORT
	implicit none
	integer :: i, j

	interface
		subroutine sub1(arr, i1)
			use modTORT
			integer*8 :: i1, arr(2,2)
		end subroutine

		subroutine sub2(arr, n, r1)
			use modTORT
			integer :: n
			real*8 :: arr(n), r1
			integer*8 :: j, k
		end subroutine
	end interface

	type (black_box), save :: a_caf_dt[0:1,0:*]
	type (black_box) :: a_dt
	type (black_box), save :: b_caf_dt(10)[*]
	type (black_box) :: b_dt(10)

	a_caf_dt%i = reshape([99,87,66,19], (/2,2/))
	a_caf_dt%r = [0.0_8, 0.0_8, 0.0_8, 0.0_8]
	a_dt%i = reshape([19,17,13,3], (/2,2/))
	a_dt%r = [0.0_8, 0.0_8, 0.0_8, 0.0_8]

	call sub1(a_caf_dt%i, 5_8)

	if ( .not. compare(a_caf_dt, a_dt) ) then
		print *, "actual", a_caf_dt
		print *, "expected", a_dt
		error stop 21
	end if


	b_caf_dt = black_box(1_8, 1.5_8)
	do j = 1, 10
		b_dt(j)%i = reshape([1,1,1,1], (/2,2/))
	end do
	do j = 1, 10
		b_dt(j)%r = [5.5_8,5.5_8,5.5_8,5.5_8] * j
	end do
	do j = 1, 10
		b_caf_dt(j)%r = -j
		call sub2(b_caf_dt(j)%r, size(b_caf_dt(j)%r), -5.5_8)
	end do

	do j = 1, 10
		if ( .not. compare(b_caf_dt(j), b_dt(j)) ) then
			print *, "actual", b_caf_dt(j)
			print *, "expected", b_dt(j)
			print *, j
			error stop 21
		end if
	end do

contains

	logical function compare(caf, dt)
		type (black_box) :: caf[*], dt
		logical :: precision_r8
		compare = .true.

		if ( any(caf%i .ne. dt%i) ) compare = .false.
		do j = 1, size(caf%r)
			if (.not. precision_r8(caf%r(j), dt%r(j)) ) compare = .false.
		end do
	end function

end


subroutine sub1(arr, i1)
	use modTORT
	integer*8 :: i1, arr(2,2)
	integer :: j, k

	do j = 1, ubound(arr, 1)
		do k = 1, ubound(arr, 2)
			arr(j,k) = arr(j,k) / i1
		end do
	end do
end subroutine

subroutine sub2(arr, n, r1)
	use modTORT
	integer :: n
	real*8 :: arr(n), r1
	integer*8 :: j, k

	do j = 1, n
		arr(j) = arr(j) * r1
	end do
end subroutine

