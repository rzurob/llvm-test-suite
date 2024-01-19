! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrUnpackDT.f
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
!*
!* - data-pointer of derived-type, a module variable with private attribute
!* - data-pointer as arg of unpack, redefined with value of unpack result
!* - data-pointer is not poly, assignment-target is the ancestor component
!*                   of data-target
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type A(k1)    ! (4)
	    integer, kind :: k1
	    integer(k1)      x
	end type

	type, extends(A) :: B    ! (4)
	    integer(k1) :: y
	end type

	logical*4 :: mask(2,1,3)

	type(A(4)), pointer, private :: fV(:,:,:)
	class(A(4)), target, allocatable :: bT(:)

	contains
	    subroutine sub(a)
	        class(A(4)) :: a(:)

		fv(2:3, (0):0, 3:5) => bT(::2)

		if ( .not. associated(fv)) error stop 11

	        fv = unpack(a, mask, fv)

	    end subroutine

	    subroutine output()
		if ( any ( lbound(fv) .ne. (/2,0,3/))) error stop 23
		if ( any ( ubound(fv) .ne. (/3,0,5/))) error stop 25
		print *, fv%x
	    end subroutine
    end module

    program main
	use m

	allocate(bT(12), source=(/ (B(4)(i, i+1), i=1,12) /) )

	mask = reshape ( (/ .false., .false., .true., .true., &
		    .false., .false. /), (/ 2,1,3 /))

	call sub( (/ A(4)(101), A(4)(-101) /) )
	call output
    end program
