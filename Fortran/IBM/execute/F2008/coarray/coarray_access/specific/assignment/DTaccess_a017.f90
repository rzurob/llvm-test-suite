!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a017.f
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign a non-coarray variable to a Derived Type coarray pointer component
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modMTL
	type A
		integer(1), pointer :: i1(:)
		integer(2), pointer :: i2
	end type
	type(A), save :: CAFA[0:2,0:*]

	type B
		integer(4), pointer :: i4(:)
		integer(8), pointer :: i8
	end type
	type(B), save :: CAFB[*]
end module


program main
	use modMTL
	implicit none

	integer(1), parameter :: mid1 = 1_1
	integer(2), parameter :: mid2 = 499_2
	integer(4), parameter :: mid4 = 10807551_4
	integer(8), parameter :: mid8 = -800851337740_8

	integer(1) :: p1, t1, pa1(:), iatgt1(5)
	integer(2) :: p2, t2, iatgt2(5)
	integer(4) :: p4, t4, iatgt4(5)
	integer(8) :: p8, t8, iatgt8(5)

	pointer :: p1, p2, p4, p8, pa1
	target  :: t1, t2, t4, t8, iatgt1, iatgt2, iatgt4, iatgt8

	iatgt1 = [integer(1) :: mid1, mid1, mid1, mid1, mid1]
	t2 = mid2
	iatgt4 = [integer(4) :: mid1, mid2, mid4, mid8, mid1]
	t8 = mid8


	! test assignment to coarrays
	CAFA%i1 => iatgt1
	CAFA%i2 => t2
	CAFB%i4 => iatgt4
	CAFB%i8 => t8

	if ( any(CAFA%i1 /= [integer(1) :: mid1, mid1, mid1, mid1, mid1]) ) then
		print *, "actual", CAFA%i1
		print *, "expected", mid1, mid1, mid1, mid1, mid1
		error stop 21
	end if

	if (CAFA%i2 /= mid2) then
		print *, "actual", CAFA%i2
		print *, "expected", mid2
		error stop 22
	end if

	if ( any(CAFB%i4 /= [integer(4) :: mid1, mid2, mid4, mid8, mid1]) ) then
		print *, "actual", CAFB%i4
		print *, "expected", mid1, mid2, mid4, mid8, mid1
		error stop 23
	end if

	if (CAFB%i8 /= mid8) then
		print *, "actual", CAFB%i8
		print *, "expected", mid8
		error stop 24
	end if


	! test assignment from coarrays (first, reset targets)
	nullify(CAFA%i1)
	nullify(CAFA%i2)
	nullify(CAFB%i4)
	nullify(CAFB%i8)

	iatgt1 = [integer(1) :: mid8, mid4, mid2, mid1, mid1]
	CAFA%i1 => iatgt1
	CAFA%i2 = mid2
	CAFB%i4 = [integer(4) :: mid4, 0, mid4, 0, mid4]
	CAFB%i8 = mid8

	pa1 => CAFA%i1
	t2 = CAFA%i2
	iatgt4 = CAFB%i4
	p8 => CAFB%i8

	if ( any(pa1 /= [integer(1) :: mid8, mid4, mid2, mid1, mid1]) ) then
		print *, "actual", p1
		print *, "expected", mid8, mid4, mid2, mid1, mid1
		error stop 31
	end if

	if ( t2 /= mid2 ) then
		print *, "actual", t2
		print *, "expected", mid2
		error stop 32
	end if

	if ( any(iatgt4 .ne. [integer(4) :: mid4, 0, mid4, 0, mid4]) ) then
		print *, "actual", iatgt4
		print *, "expected", mid4, 0, mid4, 0, mid4
		error stop 33
	end if

	if ( p8 /= mid8 ) then
		print *, "actual", t8
		print *, "expected", mid8
		error stop 34
	end if

end
