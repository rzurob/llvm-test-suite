!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a001.f
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign simple values to a Derived Type coarray's integer components
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	type obj
		integer(1) :: i1
		integer(2) :: i2
		integer(4) :: i4
		integer(8) :: i8
	end type

	integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
	integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
	integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
	integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

	integer(1) :: v1, itmp1, iatmp1(10)
	integer(2) :: v2, itmp2, iatmp2(10)
	integer(4) :: v4, itmp4, iatmp4(10)
	integer(8) :: v8, itmp8, iatmp8(10)

	type (obj), save :: caf[*], cafar(10)[*]

	v1 = min1
	v2 = min2
	v4 = min4
	v8 = min8

	! start with the minimum value for each kind:
	caf%i1 = v1
	caf%i2 = v2
	caf%i4 = v4
	caf%i8 = v8

	cafar(:)%i1 = [1,1,2,3,5,8,13,21,34,v1]
	cafar(1:9:2)%i2 = v2 			! odd elements
	cafar([2,4,6,8,10])%i4 = v4 	! even elements
	cafar(9:1:-2)%i8 = v8	 		! odd elements again

	if ((caf%i1 /= v1) .or. (caf%i2 /= v2) .or. (caf%i4 /= v4) .or. (caf%i8/=v8)) error stop 2
	if ( any(cafar(:)%i1 /= [1,1,2,3,5,8,13,21,34,v1]) ) error stop 3
	if ( any(cafar(:)%i2 /= [v2,0,v2,0,v2,0,v2,0,v2,0]) ) error stop 4
	if ( any(cafar(:)%i4 /= [0,v4,0,v4,0,v4,0,v4,0,v4]) ) error stop 5
	if ( any(cafar(:)%i8 /= [v8,0,v8,0,v8,0,v8,0,v8,0]) ) error stop 6

	! now test assignment *from* coarrays
	itmp1 = caf%i1
	itmp2 = caf%i2
	itmp4 = caf%i4
	itmp8 = caf%i8
	iatmp1 = cafar(:)%i1
	iatmp2 = cafar(:)%i2
	iatmp4 = cafar(:)%i4
	iatmp8 = cafar(:)%i8

	if ( (itmp1 /= v1) .or. (itmp2 /= v2) .or. (itmp4 /= v4) .or. (itmp8 /= v8) ) error stop 12
	if ( any(iatmp1 /= [1,1,2,3,5,8,13,21,34,v1]) ) error stop 13
	if ( any(iatmp2 /= [v2,0,v2,0,v2,0,v2,0,v2,0]) ) error stop 14
	if ( any(iatmp4 /= [0,v4,0,v4,0,v4,0,v4,0,v4]) ) error stop 15
	if ( any(iatmp8 /= [v8,0,v8,0,v8,0,v8,0,v8,0]) ) error stop 16

	! now set to max value
	v1 = max1
	v2 = max2
	v4 = max4
	v8 = max8

	caf%i1 = v1
	caf%i2 = v2
	caf%i4 = v4
	caf%i8 = v8

	! arrays will have some intermediate values, some max values
	cafar(:)%i1 = [1,1,2,3,5,8,13,21,34,v1]
	cafar(:)%i2 = mid2
	cafar(1:9:2)%i2 = v2 			! odd elements
	cafar(:)%i4 = mid4
	cafar([2,4,6,8,10])%i4 = v4 	! even elements
	cafar(:)%i8 = mid8
	cafar(9:1:-2)%i8 = v8 			! odd elements again

	if ((caf%i1 /= v1) .or. (caf%i2 /= v2) .or. (caf%i4 /= v4) .or. (caf%i8/=v8)) error stop 22
	if ( any(cafar(:)%i1 /= [1,1,2,3,5,8,13,21,34,v1]) ) error stop 23
	if ( any(cafar(:)%i2 /= [v2,mid2,v2,mid2,v2,mid2,v2,mid2,v2,mid2]) ) error stop 24
	if ( any(cafar(:)%i4 /= [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4]) ) error stop 25
	if ( any(cafar(:)%i8 /= [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8]) ) error stop 26

	itmp1 = caf%i1
	itmp2 = caf%i2
	itmp4 = caf%i4
	itmp8 = caf%i8
	iatmp1 = cafar(:)%i1
	iatmp2 = cafar(:)%i2
	iatmp4 = cafar(:)%i4
	iatmp8 = cafar(:)%i8

	if ( (itmp1 /= v1) .or. (itmp2 /= v2) .or. (itmp4 /= v4) .or. (itmp8 /= v8) ) error stop 32
	if ( any(iatmp1 /= [1,1,2,3,5,8,13,21,34,v1]) ) error stop 33
	if ( any(iatmp2 /= [v2,mid2,v2,mid2,v2,mid2,v2,mid2,v2,mid2]) ) error stop 34
	if ( any(iatmp4 /= [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4]) ) error stop 35
	if ( any(iatmp8 /= [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8]) ) error stop 36

end
