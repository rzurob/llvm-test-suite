! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/decimaledit/dtio/d322425a.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 322425)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: x(:)
    end type

    type base(k2,n2)    ! (4,20)
        integer, kind                      :: k2
        integer, len                       :: n2
        class(dataType(k2,:)), allocatable :: data
    end type
end module

program dcmlChildWrite005
use m
    type(base(4,:)), allocatable :: b1(:)
    logical(4), external :: precision_x8

    allocate (base(4,20) :: b1(12))

    b1(2) = base(4,20)(dataType(4,20)(cmplx((/(j, j=1,2)/), (/(2*j, j=1, 2)/))))

    select type (x => b1(2)%data%x)
        type is (complex)
            if (size(x) /= 2) stop 1

            if (.not. precision_x8 (x(1), cmplx(1, 2))) stop 2
            if (.not. precision_x8 (x(2), cmplx(2, 4))) stop 3
        class default
            stop 10
    end select
end
