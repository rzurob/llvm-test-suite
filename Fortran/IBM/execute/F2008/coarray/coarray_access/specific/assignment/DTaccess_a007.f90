!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a007.f
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign non-coarray Derived Type components to Derived Type coarray
!*  components (scalars and arrays of different kinds)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modDN

	type A
		integer*8 :: i
		real*8 :: r
	end type

	type B
		logical :: l
		type (A) :: a
	end type

end module


program main

	use modDN
	integer*8, parameter :: seti(5) = [99, -12345, 65238, 1000001, -1]
	real*8, parameter :: setr(5) = [12.466, -1.002, -56787.9, 123.456, 9886492.2864]
	integer*8 :: iarr(5)
	real*8 :: rarr(5)
	logical :: precision_R8

	type (A), save :: cafA(5)[1,2,*]
	type (A) :: dtA(5)
	type (B), save :: cafB[0:*]
	type (B) :: dtB


	! start with assignment to the coarray
	dtA%i = seti
	dtA%r = setr
	cafA(1)%i = dtA(1)%i
	cafA(2:5)%i = dtA(2:5)%i
	cafA%r = dtA%r

	if ( any(cafA%i .ne. dtA%i)) then
		print *, "actual", cafA%i
		print *, "expected", dtA%i
		error stop 21
	end if

	do i = 1, 5
		if (.not. precision_R8(cafA(i)%r, dtA(i)%r)) then
			print *, "actual", cafA%r
			print *, "expected", dtA%r
			print *, i
			error stop 22
		end if
	end do

	print *, floor(cafA(:)%r)
	print *, ceiling(cafA(:)%r)


	! next test assignment from the coarray
	dtA(:)%i = cafA(:)%i
	dtA(1:5:2)%r = cafA(1:5:2)%r
	dtA(2:5:2)%r = cafA(2:5:2)%r

	if ( any(dtA%i .ne. seti) ) then
		print *, "actual", dtA%i
		print *, "expected", seti
		error stop 23
	end if

	do i = 1, 5
		if ( .not. precision_R8(dtA(i)%r, setr(i)) ) then
			print *, "actual", dtA%r
			print *, "expected", setr
			print *, i
			error stop 24
		end if
	end do


	! now assignment of a coarray array
	dtB%l = .true.
	dtB%a = A(seti(1), setr(1))
	cafB%l = dtB%l
	cafB%a = dtB%a

	if (.not. cafB%l) error stop 25

	if ( cafB%a%i /= dtB%a%i ) then
		print *, "actual", cafB%a%i
		print *, "expected", dtB%a%i
		error stop 26
	end if

	if ( .not. precision_R8(cafB%a%r, dtB%a%r) ) then
		print *, "actual", cafB%a%r
		print *, "expected", dtB%a%r
		error stop 27
	end if


	! next test assignment from the coarray array
	dtB%l = cafB%l
	dtB%a%i = cafB%a%i
	dtB%a%r = cafB%a%r

	if (.not. cafB%l) error stop 28

	if (dtB%a%i /= seti(1)) then
		print *, "actual", dtB%a%i
		print *, "expected", seti(1)
		error stop 29
	end if

	if ( .not. precision_R8(dtB%a%r, setr(1)) ) then
		print *, "actual", dtB%a%r
		print *, "expected", setr(1)
		error stop 30
	end if

end
