!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p009.f
!*
!*  DATE                       : May 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray argument into intent(in) dummy argument of a procedure.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	integer(1), parameter :: a1 = 9, b1 = 99
	integer(2), parameter :: a3 = -4, b3 = 256

	type obj
		integer(1) :: i1
		integer(2) :: i2
	end type
	type (obj), save :: caf[*]

	caf = obj(0_1, 0_2)

	call twiddle1(caf%i1, 0_1, 1)
	caf%i1 = a1
	call twiddle1(caf%i1, a1, 2)
	caf%i1 = b1
	call twaddle1(caf, b1, 3)
	caf%i1 = a1
	call twaddle1(caf, a1, 4)
	caf%i1 = b1
	if (b1 /= fiddle1(caf%i1)) then
		print *, b1
		error stop 31
	end if
	caf%i1 = a1
	if (a1 /= faddle1(caf)) then
		print *, a1
		error stop 32
	end if


	call twiddle3(caf%i2, 0_2, 11)
	caf%i2 = a3
	call twiddle3(caf%i2, a3, 12)
	caf%i2 = b3
	call twaddle3(caf, b3, 13)
	caf%i2 = a3
	call twaddle3(caf, a3, 14)
	caf%i2 = b3
	if (b3 /= fiddle3(caf%i2)) then
		print *, b3
		error stop 33
	end if
	caf%i2 = a3
	if (a3 /= faddle3(caf)) then
		print *, a3
		error stop 34
	end if

contains

	subroutine twiddle1(a1, exp1, nr)
		integer(1), intent(in) :: a1
		integer(1) :: exp1
		integer :: nr

		if (a1 /= exp1) then
			print *, a1, exp1
			call fail(nr)
		end if
	end subroutine twiddle1

	subroutine twaddle1(a1, exp1, nr)
		type (obj), intent(in) :: a1[*]
		integer(1) :: exp1
		integer :: nr

		if (a1%i1 /= exp1) then
			print *, a1%i1, exp1
			call fail(nr)
		end if
	end subroutine twaddle1

	integer(1) function fiddle1(a1)
		integer(1), intent(in) :: a1
		fiddle1 = a1
	end function fiddle1

	integer(1) function faddle1(a1)
		type (obj), intent(in) :: a1[*]
		faddle1 = a1%i1
	end function faddle1


	subroutine twiddle3(a3, exp3, nr)
		integer(2), intent(in) :: a3
		integer(2) :: exp3
		integer :: nr

		if (a3 /= exp3) then
			print *, a3, exp3
			call fail(nr)
		end if
	end subroutine twiddle3

	subroutine twaddle3(a3, exp3, nr)
		type (obj), intent(in) :: a3[*]
		integer(2) :: exp3
		integer :: nr

		if (a3%i2 /= exp3) then
			print *, a3%i2, exp3
			call fail(nr)
		end if
	end subroutine twaddle3

	integer(2) function fiddle3(a3)
		integer(2), intent(in) :: a3
		fiddle3 = a3
	end function fiddle3

	integer(2) function faddle3(a3)
		type (obj), intent(in) :: a3[*]
		faddle3 = a3%i2
	end function faddle3


	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 41
	end subroutine fail

end
