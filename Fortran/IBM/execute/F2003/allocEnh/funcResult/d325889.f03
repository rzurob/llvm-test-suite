! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (defect 325889)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data
    end type

    contains

    function genBaseArray (r1, lb)
        real, intent(in) ::r1(:)
        integer, intent(in) :: lb

        type(base) genBaseArray (lb:lb+size(r1)-1)

        do i = lb, lb+size(r1)-1
            genBaseArray(i)%data = r1(i-lb+1)
        end do
    end function
end module

program funcRet001
use m
    type(base), allocatable :: b2(:)

    real r1(10:100)

    logical(4), external :: precision_r4

    r1 = [(log(i*1.0), i=10, 100)]

    b2 = genBaseArray (r1, 20)

    if ((lbound(b2,1) /= 1) .or. (ubound(b2, 1) /= 91)) error stop 1_4

    if ((lbound(genBaseArray(r1, 20), 1) /= 1) .or. &
        (ubound(genBaseArray(r1, 20), 1) /= 91)) error stop 2_4
end