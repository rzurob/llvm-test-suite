! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrDummyArgPtr.f
! opt variations: -qnol -qnodeferredlp

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
!* - data-target is type bound procedure
!* - data-pointer is dummy arg of type bound procedure
!* - lb is a passed object dummy arg
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base(n1,k1)    ! (20,4)
	integer, kind :: k1
	integer, len  :: n1
	integer(k1)   :: x
	contains
	    procedure :: func => basefunc
    end type

    contains

	function basefunc(a, p)
	    class(base(*,4)), intent(in) :: a
	    type(base(:,4)), pointer :: p(:)
	    type(base(:,4)), pointer :: basefunc(:)

	    allocate(basefunc(a%x), source = (/ (base(20,4)(i),i=1,a%x) /) )

	    if ( .not. associated(basefunc) ) stop 3
	    p( a%x : ) => basefunc(a%x/2:)

	end function

end module

    program main
	use m

	type(base(20,4)) :: b1
	type(base(:,4)), pointer :: b2(:)
	type(base(:,4)), pointer :: p1(:)

	allocate(base(20,4) :: b2(10))

        b1%x = 20

        b2(lbound(p1,1):ubound(p1,1)) => b1%func(p1)

	if ( .not. associated(p1) ) stop 13
	if ( lbound(p1,1) /= 20 ) stop 15
        if ( ubound(p1,1) /= 30 ) stop 17
	print *, p1%x

	if ( lbound(b2,1) /= 20 ) stop 21
        if ( ubound(b2,1) /= 30 ) stop 23
	print *, b2%x

    end program
