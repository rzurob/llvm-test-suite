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
!* - data-ptr assignment appears as forall statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
    	complex*8, pointer :: ptr(:,:)
        complex*8 :: tar(4,5)
    end type
end module

program main
    use m

    type(base), pointer :: b1(:)

    allocate(b1(10))

    forall(i=1:10)
	b1(i)%tar = reshape((/( cmplx((i-1)*20+j, -(i-1)*20-j, 4) , j=1,20 )/), (/4,5/))
    end forall

    forall( i = 1:10 )
	b1(i)%ptr(i:,i:) => b1(i)%tar
    end forall

    do i = 1, 10
	if ( .not. associated(b1(i)%ptr, b1(i)%tar)) error stop 3
	if ( any (lbound(b1(i)%ptr) .ne. (/ i,i/) )) error stop 5
	if ( any (ubound(b1(i)%ptr) .ne. (/ i+3,i+4/) )) error stop 7
    	write (*, '("(",f20.10,", ", f20.10, ")")') b1(i)%ptr
    	write (*, '("(",f20.10,", ", f20.10, ")")') b1(i)%ptr * cmplx(i,-i,4)
    enddo
end program
