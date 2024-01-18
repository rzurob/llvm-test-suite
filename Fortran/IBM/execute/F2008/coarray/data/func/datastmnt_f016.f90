!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : November 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test initialization with DATA statements of derived type coarray scalars (components).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	type point
		integer :: x, y, z
		real :: t
	end type
	type (point), save :: p1[*], p2[1,1:2,3,-1:1,5:9,1:*]
	type (point), save :: p3[0:2,0:2,0:*]

	data p1%x, p1%y, p1%z, p1%t/2, 3, 5, 19.5/
	data p2%x/99/, p2%y/100/, p2%z/101/, p2%t/102.25/
	data p3%x, p3%y, p3%z, p3%t /3*10, 66.78/
end module


program main
	use modFDC

	if (p1%x /= 2) then
		print *, p1%x
		error stop 21
	end if
	if (p1%y /= 3) then
		print *, p1%y
		error stop 22
	end if
	if (p1%z /= 5) then
		print *, p1%z
		error stop 23
	end if
	if (p1%t /= 19.5) then
		print *, p1%t
		error stop 24
	end if


	if (p2%x /= 99) then
		print *, p2%x
		error stop 31
	end if
	if (p2%y /= 100) then
		print *, p2%y
		error stop 32
	end if
	if (p2%z /= 101) then
		print *, p2%z
		error stop 33
	end if
	if (p2%t /= 102.25) then
		print *, p2%t
		error stop 34
	end if


	if (p3%x /= 10) then
		print *, p3%x
		error stop 41
	end if
	if (p3%y /= 10) then
		print *, p3%y
		error stop 42
	end if
	if (p3%z /= 10) then
		print *, p3%z
		error stop 43
	end if
	if (p3%t /= 66.78) then
		print *, p3%t
		error stop 44
	end if
end
