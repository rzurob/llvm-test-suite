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
	type base
	    integer*4, allocatable :: tar(:)
            integer*4, pointer :: ptr(:)
	end type
   end module

   program main
        use m

	type(base), target, allocatable :: a(:)

	a = (/ base( [1,2,3,4], null() ),base( [5,6,7,8], null()) /)

	print *, (/ (a(i)%tar, i=1,2) /)

	do i = 1, 2, 2

            a(i)%ptr(i:) => a(mod(i,2)+1)%tar
	    do j = i+3, i+3
                a(1+i)%ptr(i+1:j) => a(i)%tar
	    enddo

	enddo

        if ( .not. associated(a(1)%ptr,a(2)%tar)) error stop 1
        if ( .not. associated(a(2)%ptr, a(1)%tar(1:3))) error stop  2
	if (any(lbound(a(1)%ptr) .ne. (/1/))) error stop 3
	if (any(ubound(a(1)%ptr) .ne. (/4/))) error stop 5
	if (any(lbound(a(2)%ptr) .ne. (/2/))) error stop 7
	if (any(ubound(a(2)%ptr) .ne. (/4/))) error stop 9

	print *, (/ (a(i)%ptr, i=1,2) /)

        End program
