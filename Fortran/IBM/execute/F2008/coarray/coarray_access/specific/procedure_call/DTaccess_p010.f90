!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p010.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : May 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray argument into intent(out) dummy argument of a procedure.
!*  Note: Because the dummy args are "out", we cannot read the value passed in,
!*  so we need to verify differently.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	integer(4), parameter :: a1 = 9, b1 = 99
	integer(8), parameter :: a3 = -4, b3 = 256

	type obj
		integer(4) :: i4
		integer(8) :: i8
	end type
	type (obj), save :: caf[*]

	caf = obj(0_4, 0_8)

	call twiddle1(caf%i4, a1)
	if (caf%i4 /= a1) then
		print *, caf%i4, a1
		error stop 2
	end if

	call twiddle1(caf%i4, b1)
	if (caf%i4 /= b1) then
		print *, caf%i4, b1
		error stop 3
	end if

	call twaddle1(caf, a1)
	if (caf%i4 /= a1) then
		print *, caf%i4, a1
		error stop 4
	end if

	call twaddle1(caf, b1)
	if (caf%i4 /= b1) then
		print *, caf%i4, b1
		error stop 5
	end if

	if (a1 /= fiddle1(caf%i4, a1)) then
		print *, a1
		error stop 6
	end if
	if (b1 /= faddle1(caf, b1)) then
		print *, b1
		error stop 7
	end if

	
	call twiddle3(caf%i8, a3)
	if (caf%i8 /= a3) then
		print *, caf%i8, a3
		error stop 12
	end if

	call twiddle3(caf%i8, b3)
	if (caf%i8 /= b3) then
		print *, caf%i8, b3
		error stop 13
	end if

	call twaddle3(caf, a3)
	if (caf%i8 /= a3) then
		print *, caf%i8, a3
		error stop 14
	end if

	call twaddle3(caf, b3)
	if (caf%i8 /= b3) then
		print *, caf%i8, b3
		error stop 15
	end if

	if (a3 /= fiddle3(caf%i8, a3)) then
		print *, a3
		error stop 16
	end if
	if (b3 /= faddle3(caf, b3)) then
		print *, b3
		error stop 17
	end if

contains

	subroutine twiddle1(a1, new1)
		integer(4), intent(out) :: a1
		integer(4) :: new1
		a1 = new1
	end subroutine twiddle1

	subroutine twaddle1(a1, new1)
		type (obj), intent(out) :: a1[*]
		integer(4) :: new1
		a1%i4 = new1
	end subroutine twaddle1

	integer(4) function fiddle1(a1, new1)
		integer(4), intent(out) :: a1
		integer(4) :: new1
		a1 = new1
		fiddle1 = a1
	end function fiddle1

	integer(4) function faddle1(a1, new1)
		type (obj), intent(out) :: a1[*]
		integer(4) :: new1
		a1%i4 = new1
		faddle1 = a1%i4
	end function faddle1


	subroutine twiddle3(a3, new3)
		integer(8), intent(out) :: a3
		integer(8) :: new3
		a3 = new3
	end subroutine twiddle3

	subroutine twaddle3(a3, new3)
		type (obj), intent(out) :: a3[*]
		integer(8) :: new3
		a3%i8 = new3
	end subroutine twaddle3

	function fiddle3(a3, new3)
		integer(8), intent(out) :: a3
		integer(8) :: new3, fiddle3
		a3 = new3
		fiddle3 = a3
	end function fiddle3

	function faddle3(a3, new3)
		type (obj), intent(out) :: a3[*]
		integer(8) :: new3, faddle3
		a3%i8 = new3
		faddle3 = a3%i8
	end function faddle3

end
