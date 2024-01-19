!*******************************************************************************
!*  ============================================================================
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

	implicit none

	logical, parameter :: F = .false., T = .true.
	logical :: T1, F1
	integer :: i

	type obj
		logical(1) :: l1
		logical(2) :: l2
		logical(4) :: l4
		logical(8) :: l8
	end type
	type (obj), save :: caf[*]

	caf = obj(F, F, F, F)

	T1 = (command_argument_count() < 10)
	F1 = .not. T1

	call twiddle1(caf%l1, F1, T1, 1)
	call twiddle1(caf%l1, T1, F1, 2)
	call twaddle1(caf, F1, T1, 3)
	call twaddle1(caf, T1, F1, 4)
	if (fiddle1(caf%l1,T1)) error stop 30
	if (.not. faddle1(caf,T1)) error stop 31

	call twiddle2(caf%l2, F1, T1, 5)
	call twiddle2(caf%l2, T1, F1, 6)
	call twaddle2(caf, F1, T1, 7)
	call twaddle2(caf, T1, F1, 8)
	if (fiddle2(caf%l2,T1)) error stop 32
	if (.not. faddle2(caf,T1)) error stop 33

	call twiddle4(caf%l4, F1, T1, 9)
	call twiddle4(caf%l4, T1, F1, 10)
	call twaddle4(caf, F1, T1, 11)
	call twaddle4(caf, T1, F1, 12)
	if (fiddle4(caf%l4,T1)) error stop 34
	if (.not. faddle4(caf,T1)) error stop 35

	call twiddle8(caf%l8, F1, T1, 13)
	call twiddle8(caf%l8, T1, F1, 14)
	call twaddle8(caf, F1, T1, 15)
	call twaddle8(caf, T1, F1, 16)
	if (fiddle8(caf%l8,T1)) error stop 36
	if (.not. faddle8(caf,T1)) error stop 37

contains

	subroutine twiddle1(a1, exp1, new1, nr)
		logical(1) :: a1
		logical :: exp1, new1
		integer :: nr

		if (a1 .neqv. exp1) then
			print *, a1, exp1
			call fail(nr)
		end if
		a1 = new1
	end subroutine twiddle1

	subroutine twaddle1(a1, exp1, new1, nr)
		type (obj) :: a1[*]
		logical :: exp1, new1
		integer :: nr

		if (a1%l1 .neqv. exp1) then
			print *, a1%l1, exp1
			call fail(nr)
		end if
		a1%l1 = new1
	end subroutine twaddle1

	logical function fiddle1(a1, new1)
		logical(1) :: a1
		logical :: new1
		fiddle1 = a1
		a1 = new1
	end function fiddle1

	logical function faddle1(a1, new1)
		type (obj) :: a1[*]
		logical :: new1
		faddle1 = a1%l1
		a1%l1 = new1
	end function faddle1


	subroutine twiddle2(a2, exp2, new2, nr)
		logical(2) :: a2
		logical :: exp2, new2
		integer :: nr

		if (a2 .neqv. exp2) then
			print *, a2
			call fail(nr)
		end if
		a2 = new2
	end subroutine twiddle2

	subroutine twaddle2(a2, exp2, new2, nr)
		type (obj) :: a2[*]
		logical :: exp2, new2
		integer :: nr

		if (a2%l2 .neqv. exp2) then
			print *, a2%l2, exp2
			call fail(nr)
		end if
		a2%l2 = new2
	end subroutine twaddle2

	logical function fiddle2(a2, new2)
		logical(2) :: a2
		logical :: new2
		fiddle2 = a2
		a2 = new2
	end function fiddle2

	logical function faddle2(a2, new2)
		type (obj) :: a2[*]
		logical :: new2
		faddle2 = a2%l2
		a2%l2 = new2
	end function faddle2


	subroutine twiddle4(a4, exp4, new4, nr)
		logical(4) :: a4
		logical :: exp4, new4
		integer :: nr

		if (a4 .neqv. exp4) then
			print *, a4, exp4
			call fail(nr)
		end if
		a4 = new4
	end subroutine twiddle4

	subroutine twaddle4(a4, exp4, new4, nr)
		type (obj) :: a4[*]
		logical :: exp4, new4
		integer :: nr

		if (a4%l4 .neqv. exp4) then
			print *, a4%l4, exp4
			call fail(nr)
		end if
		a4%l4 = new4
	end subroutine twaddle4

	logical function fiddle4(a4, new4)
		logical(4) :: a4
		logical :: new4
		fiddle4 = a4
		a4 = new4
	end function fiddle4

	logical function faddle4(a4, new4)
		type (obj) :: a4[*]
		logical :: new4
		faddle4 = a4%l4
		a4%l4 = new4
	end function faddle4


	subroutine twiddle8(a8, exp8, new8, nr)
		logical(8) :: a8
		logical :: exp8, new8
		integer :: nr

		if (a8 .neqv. exp8) then
		print *, a8, exp8
		call fail(nr)
		end if
		a8 = new8
	end subroutine twiddle8

	subroutine twaddle8(a8, exp8, new8, nr)
		type (obj) :: a8[*]
		logical :: exp8, new8
		integer :: nr

		if (a8%l8 .neqv. exp8) then
		print *, a8%l8, exp8
		call fail(nr)
		end if
		a8%l8 = new8
	end subroutine twaddle8

	logical function fiddle8(a8, new8)
		logical(8) :: a8
		logical :: new8
		fiddle8 = a8
		a8 = new8
	end function fiddle8

	logical function faddle8(a8, new8)
		type (obj) :: a8[*]
		logical :: new8
		faddle8 = a8%l8
		a8%l8 = new8
	end function faddle8

	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 12
	end subroutine fail

end
