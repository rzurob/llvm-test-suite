! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrTransposeDT.f
! opt variations: -ql -qreuse=none

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
!* - pointer as arg of transpose, derived-type
!* - pointer is not poly, but target is poly with diff dynamic type
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    integer lb, ub

    type A(k1)    ! (4)
	integer, kind :: k1
	integer(k1)      x
    end type

    type, extends(A) :: B    ! (4)
	real(k1) y
    end type

    class(A(4)), target, allocatable :: tar(:)

    contains
	function func(b1)
	    type(A(4)), pointer :: b1(:,:)
	    type(A(4)), allocatable :: func(:,:)

	    if ( .not. associated(b1)) error stop 1
	    if ( any( lbound(b1) .ne. (/1,1 /) )) error stop 3
	    if ( any( ubound(b1) .ne. (/4,6 /) )) error stop 5

	    Do i = 1, ubound(b1,1)
                 print *, b1(i,:)
	    end do

	    func = transpose(b1)

	end function

end module

    program main

	use m

	type(A(4)), pointer :: p(:,:)
	type(A(4)), allocatable :: f_tar(:,:)

	lb = 4
	ub = 6

	allocate(tar(40), source = (/ ( B(4)(i,real(i)),i=1,40) /) )

	p(1:lb, 1:ub) => tar

	f_tar = func(p)

	if ( any( lbound(f_tar) .ne. (/1,1 /) )) error stop 23
	if ( any( ubound(f_tar) .ne. (/6,4 /) )) error stop 33

       	Do i = 1, ubound(f_tar,1)
     	    print *, f_tar(i,:)
	end do


    end program