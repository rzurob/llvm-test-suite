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
!* - intrinsic matmul's two args are data pointers of type integer and real
!* - type real & integer, bounds-remapping-list
!* - target is the selector of associate statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main

    	type base
	    integer, allocatable :: a(:)
    	end type

	type(base), target :: aT

	integer, pointer :: ip(:,:)

	real, pointer :: rp(:,:)

	real, target :: rt(12) = (/ ((/-1, 0, 1,2/),i=1,3)/)

	Allocate(aT%a(100), source =  &
                (/ (/((/1,2,-1/),i=1,4)/), (/(i,i=1,88)/) /))

	associate( x => aT%a )

	    ip(1:3, 1:4) => x

	    if ( .not. associated(ip) ) error stop 11
	    if ( any( lbound(ip) .ne. (/1,1 /) )) error stop 13
	    if ( any( ubound(ip) .ne. (/3,4 /) )) error stop 15

	    print *, shape(matmul(ip,reshape( (/ 0, x(13:22),0 /), (/4,3/) )))

	    print *, matmul(ip,reshape( (/ 0, x(13:22),0 /), (/4,3/) ) )

	    associate( y => ip)

	    	rp(2:5,-1:1) => rt

	    	if ( .not. associated(rp) ) error stop 21
	    	if ( any( lbound(rp) .ne. (/2,-1 /) )) error stop 23
	    	if ( any( ubound(rp) .ne. (/5,1 /) )) error stop 25

	 	print *, shape(matmul(rp,ip))

		write (*, '(4d12.4)') matmul(rp,ip)

	    end associate

	end associate

    end program