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
!* - data-pointer of derived type, as arg of pack. VECTOR is/is not presented
!* - the DT has allocatable component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type base
	    integer, allocatable :: iP(:)
	end type
    end module

    program main
	use m

	type(base), pointer ::  b2(:)
	type(base), target, allocatable :: b1(:)
	integer :: iT(10,10)
	type(base), allocatable :: res(:)

	iT = reshape((/ (i, i=1,100 ) /), (/10,10 /) )

	allocate(b2(10), source = (/ (base(iT(i,:)), i=1,10 )/) )

	b1 = b2(::2)

	b2(2:4) => b1(::2)

	if ( .not. associated(b2) ) error stop 21
	if ( any (shape(b2) .ne. (/ 3 /) )) error stop 23
	if ( lbound(b2,1) /= 2 ) error stop 25
	if ( ubound(b2,1) /= 4 ) error stop 27

	print *, (/ (b2(i)%ip, i = 2,4 )/)

	!do i = 2,4
	 !   print *, b2(i)%ip
	!end do

	res = pack(b2, (/.true.,.false., .true./))

	if ( .not. allocated(res)) error stop 29
	if ( size(res) /= 2 ) error stop 31

	!do i = 1, 2
	 !   print *, res(i)%ip
	!end do

	print *, (/ (res(i)%ip, i = 1,2 )/)

	res = pack(b2, (/.true.,.false., .true./), (/ b2(1), b2(2), &
		 base( (/ (-i,i=1,10 )/) ) /))

	if ( .not. allocated(res)) error stop 33
	if ( size(res) /= 3 ) error stop 35

	!do i = 1, 3
	 !   print *, res(i)%ip
	!end do

	print *, (/ (res(i)%ip, i = 1,3 )/)
    end program