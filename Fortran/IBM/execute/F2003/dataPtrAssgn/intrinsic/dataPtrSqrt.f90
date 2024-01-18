!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSqrt.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr as arg of elemental intrinisc function
!* - data-target is the selector of associate construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type data
	class(*), pointer :: ptr(:,:)
    end type

end module

program main

    use m

    type(data) d1
    class(*), target,allocatable :: tar(:)

    allocate(tar(16), source=real( (/(i, i=1,16) /)))

    associate(x => tar)
        d1%ptr(1:3, 1:5) => x(16:2:-1)
    end associate

    if ( .not. associated(d1%ptr)) stop 5
    if ( any(lbound(d1%ptr) .ne. (/1,1/))) stop 6
    if ( any(ubound(d1%ptr) .ne. (/3,5/))) stop 7

    select type(x=>d1%ptr)
	type is (real)
            write(*, '(5f12.8)') sqrt(x)
	class default
	    stop 10
    end select

end program
