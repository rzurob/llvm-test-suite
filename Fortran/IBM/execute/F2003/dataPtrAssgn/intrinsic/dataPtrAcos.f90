!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAcos.f
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
!* - data-ptr is a common object, data-tar is assumed-shape array
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    interface foo
        subroutine sub(tar)
            real, target :: tar(:)
        end subroutine
    end interface
end module

program main

    use m

    real, target :: tar(5) = (/0.1,0.2,0.3,0.4,0.5/)
    real, pointer :: ptr(:)
    common /blk/ ptr

    call sub(tar)

    if ( .not. associated(ptr, tar(5:1:-1))) stop 5
    if ( lbound(ptr,1) /= 2) stop 6
    if ( ubound(ptr,1) /= 6) stop 7
    write(*, '(5f12.9)') acos(ptr)

end program

subroutine sub(tar)
    real, pointer :: ptr(:)
    real, target :: tar(:)
    common /blk/ ptr

    ptr(2:) => tar(ubound(tar,1):lbound(tar,1):-1)
end subroutine
