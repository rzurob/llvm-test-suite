!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : June 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray argument to a procedure, into a dummy argument with value attribute.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	integer(1), parameter :: a1 = -1, b1 = -9
	integer(8), parameter :: a8 = 335533,  b8 = 1234567890

	type obj
		integer(1) :: i1
		integer(8) :: i8
	end type
	type (obj), save :: caf[*]

	caf = obj(0_1, 0_8)

	call twiddle1(caf%i1, 0_1, 1)
	caf%i1 = a1
	call twiddle1(caf%i1, a1, 2)
	caf%i1 = b1
	call twiddle1(caf%i1, b1, 3)
	caf%i1 = a1
	call twiddle1(caf%i1, a1, 4)
	caf%i1 = b1

	if (b1 /= fiddle1(caf%i1)) then
		print *, b1
		error stop 41
	end if
	caf%i1 = a1
	if (a1 /= fiddle1(caf%i1)) then
		print *, a1
		error stop 42
	end if


	call twiddle3(caf%i8, 0_8, 11)
	caf%i8 = a8
	call twiddle3(caf%i8, a8, 12)
	caf%i8 = b8
	call twiddle3(caf%i8, b8, 13)
	caf%i8 = a8
	call twiddle3(caf%i8, a8, 14)
	caf%i8 = b8

	if (b8 /= fiddle3(caf%i8)) then
		print *, b8
		error stop 43
	end if
	caf%i8 = a8
	if (a8 /= fiddle3(caf%i8)) then
		print *, a8
		error stop 44
	end if


contains

	subroutine twiddle1(a1, exp1, nr)
		integer(1), value :: a1
		integer(1) :: exp1
		integer :: nr

		if (a1 /= exp1) then
			print *, "actual", a1
			print *, "expected", exp1
			call fail(nr)
		end if
	end subroutine twiddle1

	integer(1) function fiddle1(a1)
		integer(1), value :: a1
		fiddle1 = a1
	end function fiddle1


	subroutine twiddle3(a8, exp8, nr)
		integer(8), value :: a8
		integer(8) :: exp8
		integer :: nr

		if (a8 /= exp8) then
			print *, "actual", a8
			print *, "expected", exp8
			call fail(nr)
		end if
	end subroutine twiddle3

	integer(8) function fiddle3(a8)
		integer(8), value :: a8
		fiddle3 = a8
	end function fiddle3


	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 12
	end subroutine fail

end
