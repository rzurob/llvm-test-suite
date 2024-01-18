!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a002.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign simple values to a Derived Type coarray's real components
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	use ieee_arithmetic

	type obj
		real(4) :: r4
		real(8) :: r8
	end type

	real(4), parameter :: min4 = tiny(0.0_4),  max4 = huge(0.0_4), mid4 = 0.76543E21
	real(8), parameter :: min8 = tiny(0.0_8),  max8 = huge(0.0_8), mid8 =-0.123456789012D123
	real(4) :: v4, rtmp4, ratmp4(10)
	real(8) :: v8, rtmp8, ratmp8(10)
	type (obj), save :: caf[*], cafar(10)[*]

	v4 = min4
	v8 = min8

	! start with the minimum value for each kind:
	caf%r4 = v4
	caf%r8 = v8
	cafar([2,4,6,8,10])%r4 = v4 	! even elements
	cafar(9:1:-2)%r8 = v8 		! odd elements again

	rtmp4 = caf%r4
	rtmp8 = caf%r8
	if ( (.not. same4(rtmp4, v4)) .or. (.not. same8(rtmp8, v8)) ) then
		print *, caf%r4, rtmp4, v4, caf%r8, rtmp8, v8
		error stop 2
	end if

	ratmp4 = cafar(:)%r4
	if (.not. all(same4(ratmp4,[0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]))) then
		print *, cafar(:)%r4
		print *, ratmp4
		print *, [0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4,0.0E0,v4]
		error stop 3
	end if

	ratmp8 = cafar(:)%r8
	if (.not. all(same8(ratmp8,[v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0])))  then
		print *, cafar(:)%r8
		print *, ratmp8
		print *, [v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0,v8,0.0D0]
		error stop 4
	end if

	! now set to max value
	v4 = max4
	v8 = max8
	
	caf%r4 = v4
	caf%r8 = v8
	cafar(:)%r4 = mid4
	cafar([2,4,6,8,10])%r4 = v4 	! even elements
	cafar(:)%r8 = mid8
	cafar(9:1:-2)%r8 = v8 		! odd elements again

	rtmp4 = caf%r4
	rtmp8 = caf%r8
	if (.not.same4(rtmp4,v4) .or. .not.same8(rtmp8,v8)) then
		print *, caf%r4, rtmp4, v4, caf%r8, rtmp8, v8
		error stop 5
	end if

	ratmp4 = cafar(:)%r4
	if (.not. all(same4(ratmp4,[mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4]))) then
		print *, cafar(:)%r4
		print *, ratmp4
		print *, [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4]
		error stop 6
	end if

	ratmp8 = cafar(:)%r8
	if (.not. all(same8(ratmp8,[v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8])))  then
		print *, cafar(:)%r8
		print *, ratmp8
		print *, [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8]
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

end
