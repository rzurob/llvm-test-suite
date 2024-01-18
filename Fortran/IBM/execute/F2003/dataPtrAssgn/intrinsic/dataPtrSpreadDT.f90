!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSpreadDT.f
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
!* - data-pointer as arg of spread, dim = 1, NC= 3
!* - data-pointer of derived-type which contains array component 
!*           with allocatable attribute.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type base
	    integer, allocatable :: iP(:) 
	end type
    end module
		
    program main
	use m

	type(base), pointer :: b1(:), b2(:)
	integer :: iT(10,10)
	type(base), allocatable :: res(:,:)

	iT = reshape((/ (i, i=1,100 ) /), (/10,10 /) )

	allocate(b2(10), source = (/ (base(iT(i,:)), i=1,10 )/) )

	b1(2:) => b2(::2)

	if ( .not. associated(b1, b2(::2)) ) stop 21
	if ( lbound(b1,1) /= 2 ) stop 25 
	if ( ubound(b1,1) /= 6 ) stop 27

	!do i = 2, 6
	 !   print *, b1(i)%ip
	!end do

	print *, (/ (b1(i)%ip, i = 2,6 )/)

	res = spread(b1, 1, 3)

	print *, shape(res)

	do i = 1, 3
	     do j = 1, 5 
		do k = 1, 10
                    print *, i, j, k, res(i,j)%ip(k)
		enddo
             end do
	enddo

    end program
