!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p013.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : June 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray argument to a procedure, into a dummy argument with optional attribute.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	integer(2), parameter :: a2 = -86, b2 = 599545
	integer(4), parameter :: a4 = 29739,  b4 = -8229353

	type obj
		integer(2) :: i2
		integer(4) :: i4
	end type
	type (obj), save :: caf[*]

	caf = obj(0_2, 0_4)

	call twiddle1(.true., caf%i2, 0_2, a2, 1)
	call twiddle1(.true., caf%i2, a2, b2, 2)
	call twaddle1(.true., caf, b2, a2, 3)
	call twaddle1(.true., caf, a2, b2, 4)
	if (b2 /= fiddle1(.true., caf%i2, a2)) then
		print *, b2
		error stop 41
	end if
	if (a2 /= faddle1(.true., caf, b2)) then
		print *, a2
		error stop 42
	end if

	call twiddle3(.true., caf%i4, 0_4, a4, 11)
	call twiddle3(.true., caf%i4, a4, b4, 12)
	call twaddle3(.true., caf, b4, a4, 13)
	call twaddle3(.true., caf, a4, b4, 14)
	if (b4 /= fiddle3(.true., caf%i4, a4)) then
		print *, b4
		error stop 43
	end if
	if (a4 /= faddle3(.true., caf, b4)) then
		print *, a4
		error stop 44
	end if
	
	call twiddle1(.false., exp2=0_2, new1=a2, nr=17)
	call twiddle1(.false., exp2=a2, new1=b2, nr=18)
	call twaddle1(.false., exp2=b2, new1=a2, nr=19)
	call twaddle1(.false., exp2=a2, new1=b2, nr=20)
	if (a2 /= fiddle1(.false., new1=a2)) then
		print *, a2
		error stop 45
	end if
	if (b2 /= faddle1(.false., new1=b2)) then
		print *, b2
		error stop 46
	end if

	call twiddle3(.false., exp4=0_4, new3=a4, nr=23)
	call twiddle3(.false., exp4=a4, new3=b4, nr=24)
	call twaddle3(.false., exp4=b4, new3=a4, nr=25)
	call twaddle3(.false., exp4=a4, new3=b4, nr=26)
	if (a4 /= fiddle3(.false., new3=a4)) then
		print *, a4
		error stop 47
	end if
	if (b4 /= faddle3(.false., new3=b4)) then
		print *, b4
		error stop 48
	end if


contains

	subroutine twiddle1(pres, a2, exp2, new1, nr)
		integer(2), optional :: a2
		integer(2) :: exp2, new1
		integer :: nr
		logical :: pres
		
		if (pres .neqv. present(a2)) call fail(100+nr)
		if (.not. present(a2)) return
		if (a2 /= exp2) then
			print *, "actual", a2
			print *, "expected", exp2
			call fail(nr)
		end if
		a2 = new1
	end subroutine twiddle1

	subroutine twaddle1(pres, a2, exp2, new1, nr)
		type (obj), optional :: a2[*]
		integer(2) :: exp2, new1
		integer :: nr
		logical :: pres
		
		if (pres .neqv. present(a2)) call fail(100+nr)
		if (.not. present(a2)) return
		if (a2%i2 /= exp2) then
			print *, "actual", a2%i2
			print *, "expected", exp2
			call fail(nr)
		end if
		a2%i2 = new1
	end subroutine twaddle1

	integer(2) function fiddle1(pres, a2, new1)
		integer(2), optional :: a2
		integer(2) :: new1
		logical :: pres
		
		if (pres .neqv. present(a2)) call fail(120)
		if (.not. present(a2)) then
			fiddle1 = new1
			return
		end if
		fiddle1 = a2
		a2 = new1
	end function fiddle1

	integer(2) function faddle1(pres, a2, new1)
		type (obj), optional :: a2[*]
		integer(2) :: new1
		logical :: pres
		
		if (pres .neqv. present(a2)) call fail(121)
		if (.not. present(a2)) then
			faddle1 = new1
			return
		end if
		faddle1 = a2%i2
		a2%i2 = new1
	end function faddle1


	subroutine twiddle3(pres, a4, exp4, new3, nr)
		integer(4), optional :: a4
		integer(4) :: exp4, new3
		integer :: nr
		logical :: pres
		
		if (pres .neqv. present(a4)) call fail(100+nr)
		if (.not. present(a4)) return
		if (a4 /= exp4) then
			print *, "actual", a4
			print *, "expected", exp4
			call fail(nr)
		end if
		a4 = new3
	end subroutine twiddle3

	subroutine twaddle3(pres, a4, exp4, new3, nr)
		type (obj), optional :: a4[*]
		integer(4) :: exp4, new3
		integer :: nr
		logical :: pres
		
		if (pres .neqv. present(a4)) call fail(100+nr)
		if (.not. present(a4)) return
		if (a4%i4 /= exp4) then
			print *, "actual", a4%i4
			print *, "expected", exp4
			call fail(nr)
		end if
		a4%i4 = new3
	end subroutine twaddle3

	integer(4) function fiddle3(pres, a4, new3)
		integer(4), optional :: a4
		integer(4) :: new3
		logical :: pres
		
		if (pres .neqv. present(a4)) call fail(122)
		if (.not. present(a4)) then
			fiddle3 = new3
			return
		end if
		fiddle3 = a4
		a4 = new3
	end function fiddle3

	integer(4) function faddle3(pres, a4, new3)
		type (obj), optional :: a4[*]
		integer(4) :: new3
		logical :: pres
		
		if (pres .neqv. present(a4)) call fail(123)
		if (.not. present(a4)) then
			faddle3 = new3
			return
		end if
		faddle3 = a4%i4
		a4%i4 = new3
	end function faddle3


	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 12
	end subroutine fail

end
