!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a015.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign allocatable non-coarray variables to a Derived Type coarray component
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modTOR
	type A
		integer(1) :: i1
		integer(2) :: i2
		integer(4) :: i4
		integer(8) :: i8
	end type
	type(A), save :: CAF, CAFAR(5)
	codimension :: CAF[100:*], CAFAR[12,-100:*]
end module


program main
	use modTOR
	implicit none

	integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
	integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
	integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
	integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

	integer(1), allocatable :: v1, iatmp1(:)
	integer(2), allocatable :: v2, iatmp2(:)
	integer(4), allocatable :: v4, iatmp4(:)
	integer(8), allocatable :: v8, iatmp8(:)

	allocate(v1, source = min1)
	allocate(v2, source = min2)
	allocate(v4, source = min4)
	allocate(v8, source = min8)

	allocate(iatmp1(5), source = [integer(1):: 1,1,2,3,v1])
	allocate(iatmp2(5), source = [integer(2):: v2,0,v2,0,v2])
	allocate(iatmp4(5), source = [integer(4):: 0,v4,0,v4,0])
	allocate(iatmp8(5), source = [integer(8):: v8,0,v8,0,v8])

	! assign to coarray
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
	
	if (any(CAFAR%i1 .ne. [integer(1):: 1,1,2,3,v1])) then
		print *, "actual", CAFAR%i1
		print *, "expected", 1,1,2,3,v1
		error stop 22
	end if
	
	if (any(CAFAR%i2 .ne. [integer(2):: v2,0,v2,0,v2])) then
		print *, "actual", CAFAR%i2
		print *, "expected", v2,0,v2,0,v2
		error stop 23
	end if
	
	if (any(CAFAR%i4 .ne. [integer(4):: 0,v4,0,v4,0])) then
		print *, "actual", CAFAR%i4
		print *, "expected", 0,v4,0,v4,0
		error stop 24
	end if
	
	if (any(CAFAR%i8 .ne. [integer(8):: v8,0,v8,0,v8])) then
		print *, "actual", CAFAR%i8
		print *, "expected", v8,0,v8,0,v8
		error stop 25
	end if

	
	! assign from coarray
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

	if (v1/=min1 .or. v2/=min2 .or. v4/=min4 .or. v8/=min8) then
		print *, "actual", v1, v2, v4, v8
		print *, "expected",  min1, min2, min4, min8
		error stop 31
	end if
	
	if (any(iatmp1 /= [integer(1):: 1,1,2,3,v1])) then
		print *, "actual", iatmp1
		print *, "expected", 1,1,2,3,v1
		error stop 32
	end if
	
	if (any(iatmp2 /= [integer(2):: v2,0,v2,0,v2])) then
		print *, "actual", iatmp2
		print *, "expected", v2,0,v2,0,v2
		error stop 33
	end if
	
	if (any(iatmp4 /= [integer(4):: 0,v4,0,v4,0])) then
		print *, "actual", iatmp4
		print *, "expected", 0,v4,0,v4,0
		error stop 34
	end if
	
	if (any(iatmp8 /= [integer(8):: v8,0,v8,0,v8])) then
		print *, "actual", iatmp8
		print *, "expected", v8,0,v8,0,v8
		error stop 35
	endif

end
