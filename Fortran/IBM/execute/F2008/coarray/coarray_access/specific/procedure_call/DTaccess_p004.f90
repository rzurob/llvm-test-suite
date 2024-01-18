!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p004.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : May 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*
!*  DESCRIPTION
!*
!*  Invoke procedures with Derived Type coarray complex component arguments (examine and modify them)
!*  Arrays are already handled in many other features, so we skip these.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	use ieee_arithmetic
	implicit none
	
	complex(4), parameter :: a4 = (tiny(0.0_4),-huge(0.0_4)), b4 = (huge(0.0_4),0.76543E21)
	complex(8), parameter :: a8 = (-tiny(0.0_8),huge(0.0_8)), b8 = (-0.123456789012D123,tiny(0.0_8))
	logical :: precision_x8, precision_x6
	
	type obj
		complex(4) :: c4
		complex(8) :: c8
	end type
	type (obj), save :: caf[*]
	
	caf = obj(0.0, 0.0)
	
	call twiddle4(caf%c4, (0.0_4,0.0_4), a4, 21)
	call twiddle4(caf%c4, a4, b4, 22)
	call twaddle4(caf, b4, a4, 23)
	call twaddle4(caf, a4, b4, 24)
	if (.not. precision_x8(b4,fiddle4(caf%c4,a4))) then
		print *, "expected", b4
		error stop 15
	end if
	if (.not. precision_x8(a4,faddle4(caf,b4))) then
		print *, "expected", a4
		error stop 16
	end if
	
	call twiddle8(caf%c8, (0.0_8,0.0_8), a8, 31)
	call twiddle8(caf%c8, a8, b8, 32)
	call twaddle8(caf, b8, a8, 33)
	call twaddle8(caf, a8, b8, 34)
	if (.not. precision_x6(b8,fiddle8(caf%c8,a8))) then
		print *, "expected", b8
		error stop 17
	end if
	if (.not. precision_x6(a8,faddle8(caf,b8))) then
		print *, "expected", a8
		error stop 18
	end if

contains

	subroutine twiddle4(a4, exp4, new4, nr)
		complex(4) :: a4, exp4, new4
		integer :: nr
		
		if (.not. precision_x8(a4,exp4)) then
			print *, a4, exp4
			call fail(nr)
		end if
		a4 = new4
	end subroutine twiddle4
	
	subroutine twaddle4(a4, exp4, new4, nr)
		type (obj) :: a4[*]
		complex(4) :: exp4, new4
		integer :: nr
		
		if (.not. precision_x8(a4%c4,exp4)) then
			print *, a4%c4, exp4
			call fail(nr)
		end if
		a4%c4 = new4
	end subroutine twaddle4
	
	complex(4) function fiddle4(a4, new4)
		complex(4) :: a4, new4
		fiddle4 = a4
		a4 = new4
	end function fiddle4
	
	complex(4) function faddle4(a4, new4)
		type (obj) :: a4[*]
		complex(4) :: new4
		faddle4 = a4%c4
		a4%c4 = new4
	end function faddle4


	subroutine twiddle8(a8, exp8, new8, nr)
		complex(8) :: a8, exp8, new8
		integer :: nr
		
		if (.not. precision_x6(a8,exp8)) then
			print *, a8, exp8
			call fail(nr)
		end if
		a8 = new8
	end subroutine twiddle8
	
	subroutine twaddle8(a8, exp8, new8, nr)
		type (obj) :: a8[*]
		complex(8) :: exp8, new8
		integer :: nr
		
		if (.not. precision_x6(a8%c8,exp8)) then
			print *, a8%c8, exp8
			call fail(nr)
		end if
		a8%c8 = new8
	end subroutine twaddle8
	
	complex(8) function fiddle8(a8, new8)
		complex(8) :: a8, new8
		fiddle8 = a8
		a8 = new8
	end function fiddle8
	
	complex(8) function faddle8(a8, new8)
		type (obj) :: a8[*]
		complex(8) :: new8
		faddle8 = a8%c8
		a8%c8 = new8
	end function faddle8
	
	subroutine fail(nr)
		integer :: nr
		print *, "Failed in test", nr
		error stop 12
	end subroutine fail

end
