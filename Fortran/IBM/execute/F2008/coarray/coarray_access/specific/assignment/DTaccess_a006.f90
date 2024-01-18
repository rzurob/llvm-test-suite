!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a006.f
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign a non-coarray Derived Type to a Derived Type coarray
!*  (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	type A
		integer :: i
		real :: r
	end type

	type B
		integer :: i, j
	end type

	integer, parameter :: mid(4) = [5, -7548, 974, -123]
	integer :: v1, v2, arr(4)

	type (A), save :: cafA[*]
	type (A) :: dtA
	type (B), save :: cafB(2,2)[*]
	type (B) :: dtB(2,2)


	! start with assignment to the coarray
	dtA = A(mid(1), real(mid(2)))
	cafA = dtA

	if ((cafA%i /= dtA%i) .or. (cafA%r /= dtA%r)) then
		print *, "actual", cafA
		print *, "expected", dtA
		error stop 21
	end if


	! next test assignment from the coarray
	v1 = mid(1)
	v2 = mid(2)
	dtA = cafA

	if ((dtA%i /= v1) .or. (dtA%r /= v2)) then
		print *, "actual", dtA
		print *, "expected", v1, v2
		error stop 22
	end if


	! now assignment of a coarray array
	dtB(:,:)%i = reshape(mid, [2,2])
	dtB(:,:)%j = reshape(mid, [2,2])
	cafB = dtB

	if ( any(cafB(:,:)%i .ne. dtB(:,:)%i) ) then
		print *, "actual", cafB
		print *, "expected", dtB
		error stop 23
	end if

	if ( any(cafB(:,:)%j .ne. dtB(:,:)%j) ) then
		print *, "actual", cafB
		print *, "expected", dtB
		error stop 24
	end if


	! next test assignment from the coarray array
	arr = mid
	dtB = cafB

	if ( any( reshape(dtB(:,:)%i, [4]) .ne. arr)) then
		print *, "actual", dtB
		print *, "expected", arr
		error stop 25
	end if

	if ( any( reshape(dtB(:,:)%j, [4]) .ne. arr)) then
		print *, "actual", dtB
		print *, "expected", arr
		error stop 26
	end if

end
