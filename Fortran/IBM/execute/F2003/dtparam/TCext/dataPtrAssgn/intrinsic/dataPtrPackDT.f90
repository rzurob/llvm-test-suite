! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrPackDT.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrPackDT.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!* - data-pointer of derived type, as arg of pack. VECTOR is/is not presented 
!* - the DT has allocatable component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type base(n1,k1)    ! (20,4)
	    integer, kind            :: k1
	    integer, len             :: n1
	    integer(k1), allocatable :: iP(:) 
	end type
    end module
		
    program main
	use m

	type(base(:,4)), pointer ::  b2(:)
	type(base(:,4)), target, allocatable :: b1(:)
	integer :: iT(10,10)
	type(base(:,4)), allocatable :: res(:)

	iT = reshape((/ (i, i=1,100 ) /), (/10,10 /) )

	allocate(b2(10), source = (/ (base(20,4)(iT(i,:)), i=1,10 )/) )

	b1 = b2(::2)

	b2(2:4) => b1(::2)

	if ( .not. associated(b2) ) stop 21
	if ( any (shape(b2) .ne. (/ 3 /) )) stop 23
	if ( lbound(b2,1) /= 2 ) stop 25 
	if ( ubound(b2,1) /= 4 ) stop 27

	print *, (/ (b2(i)%ip, i = 2,4 )/)

	!do i = 2,4 
	 !   print *, b2(i)%ip
	!end do

	res = pack(b2, (/.true.,.false., .true./))

	if ( .not. allocated(res)) stop 29
	if ( size(res) /= 2 ) stop 31

	!do i = 1, 2 
	 !   print *, res(i)%ip
	!end do

	print *, (/ (res(i)%ip, i = 1,2 )/)
	
	res = pack(b2, (/.true.,.false., .true./), (/ b2(1), b2(2), &
		 base(20,4)( (/ (-i,i=1,10 )/) ) /))

	if ( .not. allocated(res)) stop 33 
	if ( size(res) /= 3 ) stop 35

	!do i = 1, 3
	 !   print *, res(i)%ip
	!end do

	print *, (/ (res(i)%ip, i = 1,3 )/)
    end program
