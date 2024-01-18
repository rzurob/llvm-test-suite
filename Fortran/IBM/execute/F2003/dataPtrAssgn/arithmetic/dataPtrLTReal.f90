!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrLTReal.f
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
!* - data-ptr is function name
!* - data-tar is function return
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    contains
        function func(arg)
            real, pointer :: func(:)
            real, target :: arg(:)

            func(ubound(arg,1):) => arg(ubound(arg,1):lbound(arg,1):-1)

            if ( .not. associated(func, arg(10:1:-1))) stop 1
            if ( lbound(func,1) /= 10) stop 2
            if ( ubound(func,1) /= 19)  stop 3

        end function
end module

program main
    use m

    real, target, allocatable :: tar(:)
    real, pointer :: ptr(:)
    logical precision_r4

    allocate(tar(10), source = (/( real(i,4), i=1,10) /))

    ptr(1:2**3+2) => tar

    if ( .not. associated(ptr, tar)) stop 4
    if ( lbound(ptr,1) /= 1) stop 5
    if ( ubound(ptr,1) /=10)  stop 6

    ptr = func( (/ ptr(3:5), ptr(9:10), ptr(8), ptr(1:2), ptr(6:7)/) )

    if ( .not. precision_r4(ptr, real((/7,6,2,1,8,10,9,5,4,3/),4))) stop 8
    if ( .not. all(ptr < (/8,7,3,2,9,11,10,6,5,4/))) stop 9

end program

