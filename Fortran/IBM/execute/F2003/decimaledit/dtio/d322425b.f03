! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2006
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
    logical(4), external :: precision_r4

    i = 1

    allocate (b1(1))
    allocate (b1(1)%data)
    allocate (b1(1)%data%x(1), source=base(dataType(sin((/(j*1.0, j=1, i+2)/)))))

    select type (x => b1(1)%data%x(1))
        type is (base)
            select type (y => x%data%x)
                type is (real)
                    if (size(y) /= 3) error stop 1

                    do i = 1, 3
                        if (.not. precision_r4(y(i), sin(i*1.0))) error stop 2
                    end do

                class default
                    stop 20
            end select

        class default
            stop 10
    end select
end