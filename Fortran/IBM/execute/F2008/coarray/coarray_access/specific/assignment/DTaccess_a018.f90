!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a018.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign allocatable non-coarray derived type variables to a Derived Type coarray component
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	use ieee_arithmetic

	type A
		integer(2) :: i2
		character(4) :: c3
		real(4) :: r4
	end type

	type base
		integer(2) :: f1
	end type base

	type, extends (base) :: der(l)
		integer, len :: l
		character(l) :: f2
	end type der

	type, extends (der) :: der2(k)
		integer, kind :: k
		real(k) :: f3
	end type der2

	type(der2(:,4)), allocatable :: v
	type(der2(3,4)), allocatable :: a(:)

	integer(2), parameter :: par2 = -huge(0_2)-1
	character(3), parameter :: par3 = '#$%'
	integer, parameter :: A_CAP = iachar('A')
	real(4), parameter :: par4 = -huge(0_4)-1

	type(A), save :: iCAF[*], iaCAF(10)[*]

	allocate(der2(3,4):: v)
	allocate(a(10))

	! assign to coarrays
	v = der2(3,4)(par2,par3,par4)
	a = [(der2(3,4)(i,repeat(achar(A_CAP+i-1),3),1.0/i), i=1,10)]

	iCAF%i2 = v%f1
	iCAF%c3 = v%f2
	iCAF%r4 = v%f3

	iaCAF%i2 = a%f1
	iaCAF%c3 = a%f2
	iaCAF%r4 = a%f3

	if ( (iCAF%i2 /= par2) .or. (iCAF%c3 /= par3) .or. .not.same4(iCAF%r4,par4) ) then
		print *, "actual", iCAF%i2, iCAF%c3, iCAF%r4
		print *, "expected", par2, par3, par4
		error stop 21
	end if
	
	if ( any(iaCAF%i2 /= [(i,i=1,10)]) ) then
		print *, "actual", iaCAF%i2
		print *, "expected", [(i,i=1,10)]
		error stop 22
	end if
	
	if (any(iaCAF%c3 /= [(repeat(achar(A_CAP+i-1),3),i=1,10)])) then
		print *, "actual", iaCAF%c3
		print *, "expected", [(repeat(achar(A_CAP+i-1),3),i=1,10)]
		error stop 23
	end if
	
	if (any(.not.same4(iaCAF%r4,[(1.0/i,i=1,10)]))) then
		print *, "actual", iaCAF%r4
		error stop 24
	end if

	
	! assign from coarrays
	v = der2(3,4)(0,'',0.0)
	a = v

	v%f1 = iCAF%i2
	v%f2 = iCAF%c3
	v%f3 = iCAF%r4

	a%f1 = iaCAF%i2
	a%f2 = iaCAF%c3
	a%f3 = iaCAF%r4

	if (v%f1/=par2 .or. v%f2/=par3 .or. .not.same4(v%f3,par4)) then
		print *, "actual", v%f1, v%f2, v%f3
		print *, "expected", par2, par3, par4
		error stop 31
	end if
	
	if (any(a%f1 /= [(i,i=1,10)])) then
		print *, "actual", a%f1
		print *, "expected", [(i,i=1,10)]
		error stop 32
	end if
	
	if (any(a%f2 /= [(repeat(achar(A_CAP+i-1),3),i=1,10)])) then
		print *, "actual", a%f2
		print *, "expected", [(repeat(achar(A_CAP+i-1),3),i=1,10)]
		error stop 33
	end if
	
	if (any(.not.same4(a%f3,[(1.0/i,i=1,10)]))) then
		print *, "actual", a%f3
		error stop 34
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

end
