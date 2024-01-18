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
!* - data-pointer of type real & logical as args of unpack
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main

	logical*4, pointer :: mask (:,:)
	logical, allocatable, target :: mask_t(:)

	type A
	    real, pointer :: vP(:)
	    real, allocatable :: fP(:,:)
	end type

	type(A), allocatable :: a1

	allocate(a1, source = A(null(), reshape((/ (1.0, i=1,9) /), (/3,3/))))

	allocate(mask_t(10), source = (/ .true., .false., &
	     ((/.false.,.true./),i=1,3), .false., .true. /) )

	call sub(a1%vP)

	if ( .not. associated(a1%vP)) stop 21
	if ( any ( lbound(a1%vp,1) .ne. (/10/))) stop 23
	if ( any ( ubound(a1%vp,1) .ne. (/19/))) stop 25

	mask(3:5, -1:1) => mask_t

	if ( .not. associated(a1%vP)) stop 31
	if ( any ( lbound(mask) .ne. (/3,-1/))) stop 33
	if ( any ( ubound(mask) .ne. (/5,1/))) stop 35

	write (*, '(3f12.6)') unpack(vector=a1%vp, field=a1%fp, mask=mask)

	contains
	    subroutine sub(p)
	        real, pointer :: p(:)

		allocate(P(10), source = (/ ( real(i*2.0),i=1,10 ) /) )
		p(size(mask_t):) => p
	    end subroutine

    end program
