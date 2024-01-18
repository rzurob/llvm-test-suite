!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a013.f
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign a simple coarray to a Derived Type coarray component
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modEDM

	type A
		integer :: i
		character(5) :: ch
		real :: r
		complex :: cx
		logical :: l
	end type

	integer, parameter :: intish = 1080
	real, parameter :: realish = 54.2114
	character(3), parameter :: charish = "QOTSA"
	complex, parameter :: plexish = (1.2,-1.1)
	logical, parameter :: logish = .false.

	type(A), save :: CA[0:*], CARR(1,1,1,1,1,1)[1,1,1,1,1,*]

end module


program main
	use modEDM

	integer, save :: icaf[*], iarr(1,1,1,1,1,1)[1,5,6,*]
	real, save :: rcaf[1,2,*], rarr(1,1,1,1,1,1)[1:*]
	character(5), save :: chcaf[0:*], charr(1,1,1,1,1,1)[10:15,10:*]
	complex, save :: cxcaf[0:1,0:2,0:*], cxarr(1,1,1,1,1,1)[3,6,7,*]
	logical, save :: lcaf[2,3,4,1:*], larr(1,1,1,1,1,1)[*]

	icaf = intish
	rcaf = realish
	chcaf = charish
	cxcaf = plexish
	lcaf = logish

	! assign values to coarrays
	CA%i = icaf
	CA%r = rcaf
	CA%ch = chcaf
	CA%cx = cxcaf
	CA%l = lcaf

	CARR(:,:,:,:,:,:)%i = icaf
	CARR(:,:,:,:,:,:)%r = rcaf
	CARR(:,:,:,:,:,:)%ch = chcaf
	CARR(:,:,:,:,:,:)%cx = cxcaf
	CARR(:,:,:,:,:,:)%l = lcaf


	if ( (CA%i /= intish) .or. (CA%r /= realish) .or. (CA%ch /= charish) .or. (CA%cx /= plexish) .or. (CA%l .eqv. logish) ) then
		print *, CA
		error stop 15
	end if

	if ( any(reshape(CARR%i, [1]) /= [icaf]) ) then
		print *, "expected", [icaf]
		print *, "actual", CARR%i
		error stop 16
	end if

	if ( any(reshape(CARR%r, [1]) /= [rcaf]) ) then
		print *, "expected", [rcaf]
		print *, "actual", CARR%r
		error stop 17
	end if

	if ( any(reshape(CARR%ch, [1]) <> [chcaf]) ) then
		print *, "expected", [chcar]
		print *, "actual", CARR%ch
		error stop 18
	end if

	if ( any(reshape(CARR%cx, [1]) .ne. [cxcaf]) ) then
		print *, "expected", [cxcaf]
		print *, "actual", CARR%cx
		error stop 19
	end if

	if ( any(reshape(CARR%l, [1]) .neqv. [.false.]) ) then
		print *, "expected", [.false.]
		print *, "actual", CARR%l
		error stop 19
	end if

	! assign values from coarrays
	icaf = 0
	rcaf = 0.0
	chcaf = ""
	cxcaf = (0.0, 0.0)
	lcaf = .true.

	icaf = CA%i
	rcaf = CA%r
	chcaf = CA%ch
	cxcaf = CA%cx
	lcaf = CA%l

	iarr = CARR%i
	rarr = CARR%r
	charr = CARR%ch
	cxarr = CARR%cx
	larr = CARR%l

	if ( (icaf /= intish) .or. (rcaf /= realish) .or. (chcaf /= charish) .or. (cxcaf /= plexish) .or. (lcaf .eqv. logish) ) then
		print *, icaf, rcaf, chcaf, cxcaf, lcaf
		error stop 25
	end if

	if ( any(iarr /= CARR%i) ) then
		print *, iarr
		error stop 26
	end if
	if ( any(rarr /= CARR%r) )  then
		print *, rarr
		error stop 27
	end if

	if ( any(charr /= CARR%ch) )  then
		print *, charr
		error stop 28
	end if

	if ( any(cxarr /= CARR%cx) )  then
		print *, cxarr
		error stop 29
	end if

	if ( any(larr .neqv. CARR%l) )  then
		print *, ilarr
		error stop 30
	end if

end
