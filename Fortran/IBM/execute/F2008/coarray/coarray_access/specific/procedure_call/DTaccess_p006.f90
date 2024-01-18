!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p006.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : May 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke internal procedures to assign simple values to
!*  character components of dervied type coarray scalars of different lengths, both as coarray
!*  dummy arguments and non-coarray.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	character(1), parameter :: a1 = ' ',    b1 = '~'
	character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

	type item
		character(1) :: c1
		character(3) :: c3
	end type
	type (item), save :: caf[*]
	
	caf = item(' ', '   ')

	call twiddle1(caf%c1,  ' ', a1, 1)
	call twiddle1(caf%c1, a1, b1, 2)
	call twaddle1(caf, b1, a1, 3)
	call twaddle1(caf, a1, b1, 4)
	if (b1 /= fiddle1(caf%c1,a1)) then
		print *, b1
		error stop 21
	end if
	if (a1 /= faddle1(caf,b1)) then
		print *, a1
		error stop 22
	end if

	call twiddle3(caf%c3, '   ', a3, 11)
	call twiddle3(caf%c3, a3, b3, 12)
	call twaddle3(caf, b3, a3, 13)
	call twaddle3(caf, a3, b3, 14)
	if (b3 /= fiddle3(caf%c3,a3)) then
		print *, b3
		error stop 23
	end if
	if (a3 /= faddle3(caf,b3)) then
		print *, a3
		error stop 24
	end if


  contains

	subroutine twiddle1(a1, exp1, new1, nr)
		character(1) :: a1, exp1, new1
		integer :: nr
		
		if (a1 /= exp1) then
			print *, "|", a1, "|"
			print *, "|", exp1, "|"
			call fail(nr)
		end if
		a1 = new1
	end subroutine twiddle1

	subroutine twaddle1(a1, exp1, new1, nr)
		type (item) :: a1[*]
		character(1) :: exp1, new1
		integer :: nr
		
		if (a1%c1 /= exp1) then
			print *, "|", a1%c1, "|"
			print *, "|", exp1, "|"
			call fail(nr)
		end if
		a1%c1 = new1
	end subroutine twaddle1

	character(1) function fiddle1(a1, new1)
		character(1) :: a1, new1
		fiddle1 = a1
		a1 = new1
	end function fiddle1

	character(1) function faddle1(a1, new1)
		type (item) :: a1[*]
		character(1) :: new1
		faddle1 = a1%c1
		a1%c1 = new1
	end function faddle1


	subroutine twiddle3(a3, exp3, new3, nr)
		character(3) :: a3, exp3, new3
		integer :: nr

		if (a3 /= exp3) then
			print *, "|", a3, "|"
			print *, "|", exp3, "|"
			call fail(nr)
		end if
		a3 = new3
	end subroutine twiddle3

	subroutine twaddle3(a3, exp3, new3, nr)
		type (item) :: a3[*]
		character(3) :: exp3, new3
		integer :: nr
		
		if (a3%c3 /= exp3) then
			print *, "|", a3%c3, "|"
			print *, "|", exp3, "|"
			call fail(nr)
		end if
		a3%c3 = new3
	end subroutine twaddle3

	function fiddle3(a3, new3)
		character(3) :: a3, new3, fiddle3
		fiddle3 = a3
		a3 = new3
	end function fiddle3

	function faddle3(a3, new3)
		type (item) :: a3[*]
		character(3) :: new3, faddle3
		faddle3 = a3%c3
		a3%c3 = new3
	end function faddle3


	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 99
	end subroutine fail

end
