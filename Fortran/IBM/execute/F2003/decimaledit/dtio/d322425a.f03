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
    type dataType
        class(*), allocatable :: x(:)
    end type

    type base
        class (dataType), allocatable :: data
    end type
end module

program dcmlChildWrite005
use m
    type(base), allocatable :: b1(:)
    logical(4), external :: precision_x8

    allocate (b1(12))

    b1(2) = base(dataType(cmplx((/(j, j=1,2)/), (/(2*j, j=1, 2)/))))

    select type (x => b1(2)%data%x)
        type is (complex)
            if (size(x) /= 2) error stop 1

            if (.not. precision_x8 (x(1), cmplx(1, 2))) error stop 2
            if (.not. precision_x8 (x(2), cmplx(2, 4))) error stop 3
        class default
            stop 10
    end select
end