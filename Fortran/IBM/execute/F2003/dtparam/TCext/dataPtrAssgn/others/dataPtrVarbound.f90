! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrVarbound.f
! opt variations: -qnol -qnodeferredlp -qreuse=self

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
!* - lb/ub of data-pointer are loop index
!* - data-ptr appears in do-loop block.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type base(n1,k1,k2)    ! (20,4,4)
	    integer, kind            :: k1,k2
	    integer, len             :: n1
	    integer(k1), allocatable :: tar(:)
	    integer(k2), pointer     :: ptr(:)
	end type
   end module

   program main
        use m

	type(base(:,4,4)), target, allocatable :: a(:)

	a = (/ base(20,4,4)( [1,2,3,4], null() ),base(20,4,4)( [5,6,7,8], null()) /)

	print *, (/ (a(i)%tar, i=1,2) /)

	do i = 1, 2, 2

            a(i)%ptr(i:) => a(mod(i,2)+1)%tar
	    do j = i+3, i+3
                a(1+i)%ptr(i+1:j) => a(i)%tar
	    enddo

	enddo

        if ( .not. associated(a(1)%ptr,a(2)%tar)) stop 1
        if ( .not. associated(a(2)%ptr, a(1)%tar(1:3))) stop  2
	if (any(lbound(a(1)%ptr) .ne. (/1/))) stop 3
	if (any(ubound(a(1)%ptr) .ne. (/4/))) stop 5
	if (any(lbound(a(2)%ptr) .ne. (/2/))) stop 7
	if (any(ubound(a(2)%ptr) .ne. (/4/))) stop 9

	print *, (/ (a(i)%ptr, i=1,2) /)

        End program
