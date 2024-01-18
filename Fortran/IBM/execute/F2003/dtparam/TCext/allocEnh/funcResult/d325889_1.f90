! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/funcResult/d325889_1.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/16/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 325889, 2nd test case
!                               with lbound(genBaseArray(r1, 20)) uncommented in
!                               the print statement.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: data
    end type

    contains

    function genBaseArray (r1, lb)
        real, intent(in) ::r1(:)
        integer, intent(in) :: lb

        type(base(4)) genBaseArray (lb:lb+size(r1)-1)

        do i = lb, lb+size(r1)-1
            genBaseArray(i)%data = r1(i-lb+1)
        end do
    end function
end module

program funcRet001
use m
    type(base(4)), allocatable :: b2(:)

    real r1(10:100)

    logical(4), external :: precision_r4

    r1 = [(log(i*1.0), i=10, 100)]

    b2 = genBaseArray (r1, 20)

    print *, lbound(b2,1), lbound(genBaseArray(r1, 20))
end

