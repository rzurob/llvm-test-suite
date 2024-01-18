!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p003.f
!*
!*  DATE                       : May 2011
!*
!*  DESCRIPTION
!*
!*  Invoke procedures with Derived Type coarray real component arguments (examine and modify them)
!*  Arrays are already handled in many other features, so we skip these.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	use ieee_arithmetic
	implicit none

    	real(4), parameter :: min4 = tiny(0.0_4),  max4 = huge(0.0_4), mid4 = 0.76543E21
    	real(8), parameter :: min8 = tiny(0.0_8),  max8 = huge(0.0_8), mid8 =-0.123456789012D123
	logical :: precision_r4, precision_r8

	type obj
		real(4) :: r4
		real(8) :: r8
	end type
	type (obj), save :: caf[*]

	caf = obj(0.0, 0.0)

	call twiddle4(caf%r4, 0.0_4, min4, 21)
	call twiddle4(caf%r4, min4, mid4, 22)
	call twaddle4(caf, mid4, max4, 23)
	call twaddle4(caf, max4, min4, 24)

	if ( .not. precision_r4(min4,fiddle4(caf%r4,mid4)) ) then
		print *, "expected", min4
		error stop 15
	end if
	if ( .not. precision_r4(mid4,faddle4(caf,max4)) ) then
		print *, "expected", mid4
		error stop 16
	end if


	call twiddle8(caf%r8, 0.0_8, min8, 31)
	call twiddle8(caf%r8, min8, mid8, 32)
	call twaddle8(caf, mid8, max8, 33)
	call twaddle8(caf, max8, min8, 34)

	if ( .not. precision_r8(min8,fiddle8(caf%r8,mid8)) ) then
		print *, "expected", min8
		error stop 17
	end if
	if ( .not. precision_r8(mid8,faddle8(caf,max8)) ) then
		print *, "expected", mid8
		error stop 18
	end if

contains

	subroutine twiddle4(a4, exp4, new4, nr)
		real(4) :: a4, exp4, new4
		integer :: nr

		if (.not. precision_r4(a4,exp4)) then
			print *, a4, exp4
			call fail(nr)
		end if
		a4 = new4
	end subroutine twiddle4

	subroutine twaddle4(a4, exp4, new4, nr)
		type (obj) :: a4[*]
		real(4) :: exp4, new4
		integer :: nr

		if (.not. precision_r4(a4%r4,exp4)) then
			print *, a4%r4, exp4
			call fail(nr)
		end if
		a4%r4 = new4
	end subroutine twaddle4

	real(4) function fiddle4(a4, new4)
		real(4) :: a4, new4
		fiddle4 = a4
		a4 = new4
	end function fiddle4

	real(4) function faddle4(a4, new4)
		type (obj) :: a4[*]
		real(4) :: new4
		faddle4 = a4%r4
		a4%r4 = new4
	end function faddle4


	subroutine twiddle8(a8, exp8, new8, nr)
		real(8) :: a8, exp8, new8
		integer :: nr

		if (.not. precision_r8(a8,exp8)) then
			print *, a8, exp8
			call fail(nr)
		end if
		a8 = new8
	end subroutine twiddle8

	subroutine twaddle8(a8, exp8, new8, nr)
		type (obj) :: a8[*]
		real(8) :: exp8, new8
		integer :: nr

		if (.not. precision_r8(a8%r8,exp8)) then
			print *, a8%r8, exp8
			call fail(nr)
		end if
		a8%r8 = new8
	end subroutine twaddle8

	real(8) function fiddle8(a8, new8)
		real(8) :: a8, new8
		fiddle8 = a8
		a8 = new8
	end function fiddle8

	real(8) function faddle8(a8, new8)
		type (obj) :: a8[*]
		real(8) :: new8
		faddle8 = a8%r8
		a8%r8 = new8
	end function faddle8

	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 12
	end subroutine fail

end
