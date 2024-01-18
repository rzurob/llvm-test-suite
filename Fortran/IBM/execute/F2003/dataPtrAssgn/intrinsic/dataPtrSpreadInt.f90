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
!* - data-pointer of type integer appears in FORALL statement
!* - lb/ub of data-pointer is array element
!* - data-pointer as arg uf spread
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type base
	    integer, pointer :: iP(:,:)
	end type
    end module

    program main
	use m

	type super
	    integer :: iT(10,10)
	end type

	type(base), pointer ::  b2(:)
	type(base), target, allocatable :: b1(:)

	type(super), allocatable, target :: sp(:)

	allocate(sp(10))

	sp = (/ ( super (reshape((/(j,j=(i-1)*100+1,i*100) /),(/10,10/))), i=1,10 )/)

	allocate(b1(10), source = (/ (base(sp(i)%iT), i=1,10 )/) )

	allocate(b2(6))

	forall ( i = 1:6)
	    b2(i)%iP(sp(1)%it(1,1):3, 7:sp(1)%it(9,1)) => b1(i+2)%ip(2,:)
	end forall

	do i = 1, 6
	    if ( .not. associated(b2(i)%iP)) error stop 22
            if ( any ( lbound(b2(i)%iP) .ne. (/1,7/) ) ) error stop 24
            if ( any ( ubound(b2(i)%iP) .ne. (/3,9/) ) ) error stop 26
            if ( any ( shape(b2(i)%iP) .ne. (/3,3/) ) ) error stop 22
	enddo

        print *, (/ ( spread(source=b2(i)%ip, dim=1, NCOPIES=2), i=1,6 ) /)

    end program
