!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : November 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test initialization with DATA statements of derived type coarray arrays (whole).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modCFL
	type conj
		complex :: x, y
		logical :: flag
	end type
	type (conj), save :: c1(5), c2(2,2)


	data c1 /5*conj((10.0,0.25), (-1.0,-1.0), .true.)/
	data c2 /conj((1.0,0.5), (2.0,0.0), .false.), conj((3.0,-0.5), (4.0,-1.0), .true.), &
		conj((5.0,-1.5), (6.0,-2.0), .true.), conj((7.0,-2.5), (8.0,-3.0), .false.)/
end module


program main
	use modCFL
	call sub0()
end


subroutine sub0()
	use modCFL

	if ( any(real(c1%x) /= [10.0,10.0,10.0,10.0,10.0]) ) then
		print *, c1%x
		error stop 21
	end if
	if ( any(imag(c1%x) /= [0.25,0.25,0.25,0.25,0.25]) ) then
		print *, c1%x
		error stop 22
	end if

	if ( any(real(c1%y) /= [-1.0,-1.0,-1.0,-1.0,-1.0]) ) then
		print *, c1%y
		error stop 23
	end if
	if ( any(imag(c1%y) /= [-1.0,-1.0,-1.0,-1.0,-1.0]) ) then
		print *, c1%y
		error stop 24
	end if

	if ( any(c1%flag .eqv. [.false., .false., .false., .false., .false.]) ) then
		print *, c1%flag
		error stop 25
	end if


	if ( any(reshape(real(c2%x), [4]) /= [1.0,3.0,5.0,7.0]) ) then
		print *, reshape(c2%x, [4])
		error stop 31
	end if
	if ( any(reshape(imag(c2%x), [4]) /= [0.5,-0.5,-1.5,-2.5]) ) then
		print *, reshape(c2%x, [4])
		error stop 32
	end if

	if ( any(reshape(real(c2%y), [4]) /= [2.0,4.0,6.0,8.0]) ) then
		print *, reshape(c2%y, [4])
		error stop 33
	end if
	if ( any(reshape(imag(c2%y), [4]) /= [0.0,-1.0,-2.0,-3.0]) ) then
		print *, reshape(c2%y, [4])
		error stop 34
	end if

	if ( any(reshape(c2%flag, [4]) .eqv. [.true., .false., .false., .true.]) ) then
		print *, reshape(c2%flag, [4])
		error stop 35
	end if

end subroutine
