!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEQCmplx1.f
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - lb/ub of data-ptr is constant or expr
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    class(*), pointer :: p(:,:)
end module

program main

    use m

    integer, parameter :: lb = 1, ub=10
    integer, target :: val(20)
    complex, allocatable, target :: tar(:)

    data (val(i), i= lb,ub ) /1,2,3,4,5,6,7,8,9,10/, &
       (val(i), i=ub+1,ub+10) /10,9,8,7,6,5,4,3,2,1/

    tar = cmplx(val(::2), -val(2::2), 4)

    p(lb:size(tar)/ub, min(lb,-4):ub/2) => tar

    if ( .not. associated(p)) stop 1
    if ( any (lbound(p) .ne. (/1,-4 /))) stop 2
    if ( any (ubound(p) .ne. (/1,5 /))) stop 3

    select type(p)
        type is (complex)
            write (*, '("(",f10.6,", ", f10.6, ")")') p
	    print *, p .eq. reshape(tar, (/1,10/))
        class default
            stop 5
    end select

end program
