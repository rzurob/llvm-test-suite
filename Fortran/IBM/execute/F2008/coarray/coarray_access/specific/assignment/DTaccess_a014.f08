!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign pointer non-coarray variables to a Derived Type coarray component
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modCGY
	type A
		integer(1) :: i1
		integer(2) :: i2
		integer(4) :: i4
		integer(8) :: i8
	end type
	type(A), save :: CAF[-2:-1,-1:*], CAFAR(10)[9:*]
end module


program main
	use modCGY
	implicit none

	integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
	integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
	integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
	integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

	integer(1) :: v1, itmp1, iatmp1(:), iatgt1(10)
	integer(2) :: v2, itmp2, iatmp2(:), iatgt2(10)
	integer(4) :: v4, itmp4, iatmp4(:), iatgt4(10)
	integer(8) :: v8, itmp8, iatmp8(:), iatgt8(10)

	pointer :: v1, iatmp1, v2, iatmp2, v4, iatmp4, v8, iatmp8
	target  :: itmp1, itmp2, itmp4, itmp8, iatgt1, iatgt2, iatgt4, iatgt8

	allocate(v1, source = min1)
	allocate(v2, source = min2)
	allocate(v4)
	allocate(v8)
	v4 = min4
	v8 = min8

	allocate(iatmp1(10), source = [integer(1):: 1,1,2,3,5,8,13,21,34,v1])
	allocate(iatmp2(10), source = [integer(2):: v2,0,v2,0,v2,0,v2,0,v2,0])
	allocate(iatmp4(10), source = [integer(4):: 0,v4,0,v4,0,v4,0,v4,0,v4])
	allocate(iatmp8(10), source = [integer(8):: v8,0,v8,0,v8,0,v8,0,v8,0])


	! test assignment to coarrays
	CAF%i1 = v1
	CAF%i2 = v2
	CAF%i4 = v4
	CAF%i8 = v8

	CAFAR%i1 = iatmp1
	CAFAR%i2 = iatmp2
	CAFAR%i4 = iatmp4
	CAFAR%i8 = iatmp8

	if ( (CAF%i1 /= min1) .or. (CAF%i2 /= min2) .or. (CAF%i4 /= min4) .or. (CAF%i8 /= min8) ) then
		print *, "actual", CAF
		print *, "expected", min1, min2, min4, min8
		error stop 21
	end if

	if (any(CAFAR%i1 /= [integer(1) :: 1,1,2,3,5,8,13,21,34,v1])) then
		print *, "actual", CAFAR%i1
		print *, "expected", 1,1,2,3,5,8,13,21,34,v1
		error stop 22
	end if

	if (any(CAFAR%i2 /= [integer(2) :: v2,0,v2,0,v2,0,v2,0,v2,0])) then
		print *, "actual", CAFAR%i2
		print *, "expected", v2,0,v2,0,v2,0,v2,0,v2,0
		error stop 23
	end if

	if (any(CAFAR%i4 /= [integer(4) :: 0,v4,0,v4,0,v4,0,v4,0,v4])) then
		print *, "actual", CAFAR%i4
		print *, "expected", 0,v4,0,v4,0,v4,0,v4,0,v4
		error stop 24
	end if

	if (any(CAFAR%i8 /= [integer(8) :: v8,0,v8,0,v8,0,v8,0,v8,0])) then
		print *, "actual", CAFAR%i8
		print *, "expected", v8,0,v8,0,v8,0,v8,0,v8,0
		error stop 25
	end if


	! test assignment from coarrays (first, reset targets)
	v1 = 0
	v2 = 0
	v4 = 0
	v8 = 0
	iatmp1 = 0
	iatmp2 = 0
	iatmp4 = 0
	iatmp8 = 0

	v1 = CAF%i1
	v2 = CAF%i2
	v4 = CAF%i4
	v8 = CAF%i8
	iatmp1 = CAFAR%i1
	iatmp2 = CAFAR%i2
	iatmp4 = CAFAR%i4
	iatmp8 = CAFAR%i8

	if ( (v1 /= min1) .or. (v2 /= min2) .or. (v4 /= min4) .or. (v8 /= min8) ) then
		print *, "actual", v1, v2, v4, v8
		print *, "expected", min1, min2, min4, min8
		error stop 31
	end if

	if ( any(iatmp1 /= [integer(1) :: 1,1,2,3,5,8,13,21,34,v1]) ) then
		print *, "actual", iatmp1
		print *, "expected", 1,1,2,3,5,8,13,21,34,v1
		error stop 32
	end if

	if ( any(iatmp2 /= [integer(2) :: v2,0,v2,0,v2,0,v2,0,v2,0]) ) then
		print *, "actual", iatmp2
		print *, "expected", v2,0,v2,0,v2,0,v2,0,v2,0
		error stop 33
	end if

	if ( any(iatmp4 /= [integer(4) :: 0,v4,0,v4,0,v4,0,v4,0,v4]) ) then
		print *, "actual", iatmp4
		print *, "expected", 0,v4,0,v4,0,v4,0,v4,0,v4
		error stop 34
	end if

	if ( any(iatmp8 /= [integer(8) :: v8,0,v8,0,v8,0,v8,0,v8,0]) ) then
		print *, "actual", iatmp8
		print *, "expected", v8,0,v8,0,v8,0,v8,0,v8,0
		error stop 35
	end if


	! try targets - first release already allocated space, then set up pointers and set to max value
	deallocate(v1, iatmp1, v2, iatmp2, v4, iatmp4, v8, iatmp8)

	v1 => itmp1
	v2 => itmp2
	v4 => itmp4
	v8 => itmp8
	iatmp1 => iatgt1
	iatmp2 => iatgt2
	iatmp4 => iatgt4
	iatmp8 => iatgt8

	v1 = max1
	v2 = max2
	v4 = max4
	v8 = max8

	CAF%i1 = v1
	CAF%i2 = v2
	CAF%i4 = v4
	CAF%i8 = v8

	iatmp1 = [integer(1):: 1,1,2,3,5,8,13,21,34,v1]
	iatmp2 = mid2
	iatmp2(1:9:2) = v2 			! odd elements
	iatmp4 = mid4
	iatmp4([2,4,6,8,10]) = v4 	! even elements
	iatmp8 = mid8
	iatmp8(9:1:-2) = v8 		! odd elements again

	CAFAR%i1 = iatmp1
	CAFAR%i2 = iatmp2
	CAFAR%i4 = iatmp4
	CAFAR%i8 = iatmp8

	if ( (CAF%i1 /= v1) .or. (CAF%i2 /= v2) .or. (CAF%i4 /= v4) .or. (CAF%i8 /= v8) ) then
		print *, "actual", CAF
		print *, "expected", v1, v2, v4, v8
		error stop 41
	end if

	if ( any(CAFAR%i1 /= [integer(1):: 1,1,2,3,5,8,13,21,34,v1]) ) then
		print *, "actual", CAFAR%i1
		print *, "expected", 1,1,2,3,5,8,13,21,34,v1
		error stop 42
	end if

	if ( any(CAFAR%i2 /= [v2,mid2,v2,mid2,v2,mid2,v2,mid2,v2,mid2]) ) then
		print *, "actual", CAFAR%i2
		print *, "expected", v2,mid2,v2,mid2,v2,mid2,v2,mid2,v2,mid2
		error stop 43
	end if

	if ( any(CAFAR%i4 /= [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4]) ) then
		print *, "actual", CAFAR%i4
		print *, "expected", mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4
		error stop 44
	end if

	if ( any(CAFAR%i8 /= [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8]) ) then
		print *, "actual", CAFAR%i8
		print *, "expected", v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8
		error stop 45
	end if

end
