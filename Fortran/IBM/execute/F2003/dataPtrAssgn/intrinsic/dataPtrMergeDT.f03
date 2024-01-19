!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer of derived-type, as arg of MERGE
!* - array component element as lb/ub of data-pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main

	type base
	    integer, allocatable :: iP(:)
	end type

	type(base), pointer :: b1(:), b2(:)

	integer :: iT(10,10)

	type(base), allocatable :: res(:)

	iT = reshape((/ (i, i=1,100 ) /), (/10,10 /) )

	allocate(b2(10), source = (/ (base(iT(i,:)), i=1,10 )/) )

	do i = 1, 10
	    print *, b2(i)%ip
	end do

!	print *, (/ (b2(i)%ip, i=1,10)/)

	b1(b2(1)%ip(1):b2(4)%ip(1)) => b2(8:1:-2)

	if ( .not. associated(b1) ) error stop 12
	if ( lbound(b1,1) /= 1) error stop 15
	if ( ubound(b1,1) /= 4) error stop 18

	do i = b2(1)%ip(1), b2(4)%ip(1)
	    print *, b1(i)%ip
	end do

	res = merge(b1,b2(1:7:2),(/.true., .false., .true., .false./))

	!do i = b2(1)%ip(1), b2(4)%ip(1)
	 !   print *, res(i)%ip
	!end do
	print *, (/ (res(i)%ip, i=1,4)/)

    end program
