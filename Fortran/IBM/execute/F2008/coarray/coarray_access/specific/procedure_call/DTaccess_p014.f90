!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_p014.f
!*
!*  DATE                       : June 2011
!*
!*  DESCRIPTION
!*
!*  Pass a Derived Type coarray to a recursive procedure.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	real(8) :: res1
	integer(8) :: res2

	type obj
		integer(8) :: i8
		real(8) :: r8
	end type
	type (obj), save :: caf[*]

	caf = obj(8_8, 10.0_8)

	res1 = fun0(caf%r8)
	res2 = fun1(caf%i8)

	print *, res1, res2

	if (res1 /= 3628800.0_8) then
		print *, res1
		error stop 21
	end if
	if (res2 /= 34) then
		print *, res2
		error stop 22
	end if

contains

	recursive function fun0(a1) result(out)
		real(8) :: a1, out

		if (a1 == 0) then
			out = 1.0_8
		else
			out = real(a1 * fun0(a1 - 1), 8)
		end if
	end function

	recursive function fun1(a1) result(out)
		integer(8) :: a1, out

		if (a1 == 1) then
			out = 1
		else if (a1 == 0) then
			out = 1
		else
			out = fun1(a1 - 1) + fun1(a1 - 2)
		end if
	end function

end
