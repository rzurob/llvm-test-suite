! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/25/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               miscellaneous (defect 338409)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real data(n)

        contains

        procedure :: convert => convert2Array
    end type

    contains

    function convert2Array (b1)
        implicit none
        class(base(*)), intent(in) :: b1

        class(base(1)), pointer :: convert2Array(:)

        integer i

        allocate (convert2Array(b1%n))

        do i = 1, b1%n
            convert2Array(i)%data(1) = b1%data(i)
        end do
    end function
end module

use m
    class(base(:)), pointer :: b1, b2(:)

    logical(4), external :: precision_r4

    allocate (b1, source=base(10)([(i, i = 1, 10)]))

    b2 => b1%convert()

    if (.not. associated(b2)) error stop 1_4

    if (size(b2) /= 10) error stop 2_4

    if (b2%n /= 1) error stop 3_4

    do i = 1, 10
        if (.not. precision_r4 (b2(i)%data(1), i*1.0_4)) error stop 4_4
    end do
end
