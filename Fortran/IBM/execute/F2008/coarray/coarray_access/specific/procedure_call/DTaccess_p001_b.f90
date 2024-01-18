!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p001_b.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : June 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray arrays and array components to a procedure which modifies it.
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
		subroutine sub1(caf, i1, r1)
			use modTORT
			type (black_box), intent(inout) :: caf[*]
			integer*8 :: i1
			real*8 :: r1
		end subroutine
	
		subroutine sub2(caf, i1, r1)
			use modTORT
			type (black_box), intent(inout) :: caf(10)[1,1,0:*]
			integer*8 :: i1
			real*8 :: r1
		end subroutine
	end interface
	
	type (black_box), save :: a_caf_dt[0:1,0:*]
	type (black_box) :: a_dt
	type (black_box), save :: b_caf_dt(10)[*]
	type (black_box) :: b_dt(10)

	a_caf_dt = black_box(1_8, 1.5_8)
	a_dt%r = [3.625_8, 5.75_8, 7.875_8, 10.0_8]
	a_dt%i = reshape([5,5,5,5], (/2,2/))
		
	call sub1(a_caf_dt, 5_8, 2.125_8)
	if ( .not. compare(a_caf_dt, a_dt) ) then
		print *, "actual", a_caf_dt
		print *, "expected", a_dt
		error stop 21
	end if
	
	
	b_caf_dt = black_box(1_8, 1.5_8)
	do j = 1, 10
		b_dt(j)%r = -4.0_8
	end do
	do j = 1, 10
		b_dt(j)%i = j
	end do
	
	call sub2(b_caf_dt, 12_8, -5.5_8)
	
	do j = 1, 10
		if ( .not. compare(b_caf_dt(j), b_dt(j)) ) then
			print *, "actual", b_caf_dt
			print *, "expected", b_dt
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


subroutine sub1(caf, i1, r1)
	use modTORT
	type (black_box), intent(inout) :: caf[*]
	integer*8 :: i1
	integer :: j, k
	real*8 :: r1
	
	do j = 1, size(caf%r)
		caf%r(j) = caf%r(j) + (r1 * j)
	end do
	
	do j = 1, 2
		do k = 1, 2
			caf%i(j,k) = caf%i(j,k) * i1
		end do
	end do
end subroutine

subroutine sub2(caf, i1, r1)
	use modTORT
	type (black_box), intent(inout) :: caf(10)[1,1,0:*]
	integer*8 :: i1
	real*8 :: r1
	integer*8 :: j, k
	
	do j = 1, 10
		caf(j)%i = mod(j, i1)
	end do
	
	do j = 1, 10
		caf(j)%r = caf(j)%r + r1
	end do
end subroutine

