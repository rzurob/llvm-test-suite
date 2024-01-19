!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : May 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray argument into intent(inout) dummy argument of a procedure.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	real(4), parameter :: a1 = -12.4355, b1 = 19.9635
	real(8), parameter :: a3 = 0.9769590,  b3 = 255.08906
	logical :: precision_r4, precision_r8

	type obj
		real(4) :: r4
		real(8) :: r8
	end type
	type (obj), save :: caf[*]

	caf = obj(0.0_4, 0.0_8)

	call twiddle1(caf%r4, 0.0_4, a1, 1)
	call twiddle1(caf%r4, a1, b1, 2)
	call twaddle1(caf, b1, a1, 3)
	call twaddle1(caf, a1, b1, 4)
	if ( .not. precision_r4(b1, fiddle1(caf%r4,a1)) ) then
		print *, b1
		error stop 31
	end if
	if ( .not. precision_r4(a1, faddle1(caf,b1)) ) then
		print *, a1
		error stop 32
	end if

	call twiddle3(caf%r8, 0.0_8, a3, 11)
	call twiddle3(caf%r8, a3, b3, 12)
	call twaddle3(caf, b3, a3, 13)
	call twaddle3(caf, a3, b3, 14)
	if ( .not. precision_r8(b3, fiddle3(caf%r8,a3)) ) then
		print *, b3
		error stop 33
	end if
	if ( .not. precision_r8(a3, faddle3(caf,b3)) ) then
		print *, a3
		error stop 34
	end if

contains

	subroutine twiddle1(a1, exp1, new1, nr)
		real(4), intent(inout) :: a1
		real(4) :: exp1, new1
		integer :: nr

		if (a1 /= exp1) then
			print *, a1, exp1
			call fail(nr)
		end if
		a1 = new1
	end subroutine twiddle1

	subroutine twaddle1(a1, exp1, new1, nr)
		type (obj), intent(inout) :: a1[*]
		real(4) :: exp1, new1
		integer :: nr

		if (a1%r4 /= exp1) then
			print *, a1%r4, exp1
			call fail(nr)
		end if
		a1%r4 = new1
	end subroutine twaddle1

	real(4) function fiddle1(a1, new1)
		real(4), intent(inout) :: a1
		real(4) :: new1
		fiddle1 = a1
		a1 = new1
	end function fiddle1

	real(4) function faddle1(a1, new1)
		type (obj), intent(inout) :: a1[*]
		real(4) :: new1
		faddle1 = a1%r4
		a1%r4 = new1
	end function faddle1


	subroutine twiddle3(a3, exp3, new3, nr)
		real(8), intent(inout) :: a3
		real(8) :: exp3, new3
		integer :: nr

		if (a3 /= exp3) then
			print *, a3, exp3
			call fail(nr)
		end if
		a3 = new3
	end subroutine twiddle3

	subroutine twaddle3(a3, exp3, new3, nr)
		type (obj), intent(inout) :: a3[*]
		real(8) :: exp3, new3
		integer :: nr

		if (a3%r8 /= exp3) then
			print *, a3%r8, exp3
			call fail(nr)
		end if
		a3%r8 = new3
	end subroutine twaddle3

	real(8) function fiddle3(a3, new3)
		real(8), intent(inout) :: a3
		real(8) :: new3
		fiddle3 = a3
		a3 = new3
	end function fiddle3

	real(8) function faddle3(a3, new3)
		type (obj), intent(inout) :: a3[*]
		real(8) :: new3
		faddle3 = a3%r8
		a3%r8 = new3
	end function faddle3


	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 41
	end subroutine fail

end
