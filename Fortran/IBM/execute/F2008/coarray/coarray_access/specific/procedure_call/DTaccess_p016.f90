!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : June 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray component that is an array section.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modTRITE

	type black_box
		integer*8 :: i(4,4)
		real*8 :: r(0:9)
	end type

end module


program main

	use modTRITE
	implicit none
	integer :: i, j

	interface
		subroutine sub1(arr, i1)
			use modTRITE
			integer*8 :: i1, arr(2,2)
		end subroutine

		subroutine sub2(arr, n, r1)
			use modTRITE
			integer :: n
			real*8 :: arr(n), r1
			integer*8 :: j, k
		end subroutine
	end interface

	integer, parameter :: chunk = 10
	type (black_box), save :: a_caf_dt[0:1,0:*]
	type (black_box) :: a_dt
	type (black_box), save :: b_caf_dt(chunk)[*]
	type (black_box) :: b_dt(chunk)


	a_caf_dt = black_box(0_8, 0.0_8)
	a_caf_dt%i = reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], (/4,4/))
	a_caf_dt%r = 0.0_8
	a_dt%i = reshape([1,2,3,4,5,6,7,8,45,50,11,12,65,70,15,16], (/4,4/))
	a_dt%r = 0.0_8

	call sub1(a_caf_dt%i(1:2,3:4), 5_8)

	if ( .not. compare(a_caf_dt, a_dt) ) then
		print *, "actual", a_caf_dt
		print *, "expected", a_dt
		error stop 21
	end if


	b_caf_dt = black_box(0_8, 0.0_8)
	do j = 1, chunk
		b_dt(j)%i = 0_8
	end do
	do j = 1, chunk
		b_dt(j)%r = 0.0_8
		b_dt(j)%r(2:8:2) = -10.0_8
	end do
	do j = 1, chunk
		call sub2(b_caf_dt(j)%r(2:8:2), 4, -10.000_8)
	end do

	do j = 1, chunk
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
		do j = 0, size(caf%r) - 1
			if (.not. precision_r8(caf%r(j), dt%r(j)) ) compare = .false.
		end do
	end function

end


subroutine sub1(arr, i1)
	use modTRITE
	integer*8 :: i1, arr(2,2)
	integer :: j, k

	do j = 1, ubound(arr, 1)
		do k = 1, ubound(arr, 2)
			arr(j,k) = arr(j,k) * i1
		end do
	end do
end subroutine

subroutine sub2(arr, n, r1)
	use modTRITE
	integer :: n
	real*8 :: arr(n), r1
	integer*8 :: j, k

	arr = r1
end subroutine
