!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a003.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign simple values to a Derived Type coarray's complex components
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	use ieee_arithmetic
	implicit none

	type obj
		complex(4) :: x4
		complex(8) :: x8
	end type

	complex(4), parameter :: a4 = (tiny(0.0_4),-huge(0.0_4)), b4 = (huge(0.0_4),0.76543E21)
	complex(8), parameter :: a8 = (-tiny(0.0_8),huge(0.0_8)), b8 = (-0.123456789012D123,tiny(0.0_8))
	complex(4) :: v4, ztmp4, zatmp4(10)
	complex(8) :: v8, ztmp8, zatmp8(10)
	type (obj), save :: caf[*], cafar(10)[*]

	v4 = 0.0
	v8 = 0.0D0
	v4 = a4
	v8 = a8

	! start with the minimum value for each kind:
	caf%x4 = v4
	caf%x8 = v8
	cafar([2,4,6,8,10])%x4 = v4 	! even elements
	cafar(9:1:-2)%x8 = v8 		! odd elements again

	ztmp4 = caf%x4
	ztmp8 = caf%x8
	if ((.not. same4z(ztmp4,v4)) .or. (.not. same8z(ztmp8,v8)) ) then
		print *, caf%x4, ztmp4, v4, caf%x8, ztmp8, v8
		error stop 2
	end if

	zatmp4 = cafar(:)%x4
	if (.not. all(same4z(zatmp4,[0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]))) then
		print *, cafar(:)%x4
		print *, zatmp4
		print *, [0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]
		error stop 3
	end if

	zatmp8 = cafar(:)%x8
	if (.not. all(same8z(zatmp8,[v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0])))  then
		print *, cafar(:)%x8
		print *, zatmp8
		print *, [v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0]
		error stop 4
	end if

    ! now set to max value
	v4 = b4
	v8 = b8
	caf%x4 = v4
	caf%x8 = v8

	cafar(:)%x4 = a4
	cafar([2,4,6,8,10])%x4 = v4 		! even elements
	cafar(:)%x8 = a8
	cafar(9:1:-2)%x8 = v8 			! odd elements again

	ztmp4 = caf%x4
	ztmp8 = caf%x8
	if ( (.not. same4z(ztmp4,v4)) .or. (.not. same8z(ztmp8,v8)) ) then
		print *, caf%x4, ztmp4, v4, caf%x8, ztmp8, v8
		error stop 5
	end if

	zatmp4 = cafar(:)%x4
	if (.not. all(same4z(zatmp4,[a4,v4,a4,v4,a4,v4,a4,v4,a4,v4]))) then
		print *, cafar(:)%x4
		print *, zatmp4
		print *, [a4,v4,a4,v4,a4,v4,a4,v4,a4,v4]
		error stop 6
	end if

	zatmp8 = cafar(:)%x8
	if (.not. all(same8z(zatmp8,[v8,a8,v8,a8,v8,a8,v8,a8,v8,a8])))  then
		print *, cafar(:)%x8
		print *, zatmp8
		print *, [v8,a8,v8,a8,v8,a8,v8,a8,v8,a8]
		error stop 7
	end if

contains

	elemental logical function same4(a1,a2)
		real(4), intent(in) :: a1, a2
		real(4) :: r1, r2
		same4 = .true.
		r1 = a1
		r2 = a2
		
		! covers exact equality, Inf and NaN:
		if (r1 == r2 .or. ieee_is_nan(r1) .and. ieee_is_nan(r1)) return
		if (.not.ieee_is_normal(r1) .or. .not.ieee_is_normal(r2)) then
			r1 = 1e20 * r1
			r2 = 1e20 * r2
		end if
		
		! covers approximate equality:
		same4 = abs(r1 - r2) <= abs((r1*0.5E-5 + r2*0.5E-5)) ! avoiding overflow on max
	end function same4

	elemental logical function same8(a1,a2)
		real(8), intent(in) :: a1, a2
		real(8) :: r1, r2
		same8 = .true.
		r1 = a1
		r2 = a2
		
		! covers exact equality, Inf and NaN:
		if (r1 == r2 .or. ieee_is_nan(r1) .and. ieee_is_nan(r1)) return
		if (.not.ieee_is_normal(r1) .or. .not.ieee_is_normal(r2)) then
			r1 = 1e40 * r1
			r2 = 1e40 * r2
		end if
		
		! covers approximate equality:
		same8 = abs(r1 - r2) <= abs((r1*0.5D-14 + r2*0.5D-14)) ! avoiding overflow on max
	end function same8

	elemental logical function same4z(a1,a2)
		complex(4), intent(in) :: a1, a2
		same4z = same4(real(a1),real(a2)) .and. same4(aimag(a1),aimag(a2))
	end function same4z

	elemental logical function same8z(a1,a2)
		complex(8), intent(in) :: a1, a2
		same8z = same8(real(a1),real(a2)) .and. same8(aimag(a1),aimag(a2))
	end function same8z

end
