! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/04/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 338773: prefix causes
!                               ASTI ICE)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) data(n)

        contains

        procedure :: select => getDataWithCond
    end type

    abstract interface
        logical function compare4 (b1, b2)
            import
            class(base(4,1)), intent(in) :: b1, b2
        end function

        logical function compare8 (b1, b2)
        import
            class(base(8,1)), intent(in) :: b1, b2
        end function
    end interface

    contains

    class(base(4,:)) function getDataWithCond (b1, proc, b2)
        class(base(4,*)), intent(in) :: b1
        class(base(4,1)), intent(in) :: b2
        procedure(compare4) proc

        pointer getDataWithCond

        type (base(4,1)) temp
        logical mask(b1%n)

        do i = 1, b1%n
            temp%data = b1%data(i)

            mask(i) = proc(temp, b2)
        end do

        allocate (base(4, count(mask)) :: getDataWithCond)

        getDataWithCond%data = pack (b1%data, mask)
    end function
end module

use m
    type(base(4,:)), allocatable :: b1
    class(base(4,:)), pointer :: b2

    procedure(compare4) b1GTb2

    logical(4), external :: precision_r4

    allocate (base(4,1000) :: b1)

    b1%data(:400) = -log([(i*1.0, i = 1, 400)])
    b1%data(601:) = log([(i*1.0, i = 1, 400)])
    b1%data(401:600) = [(i, i = 1, 200)]

    b2 => b1%select (b1GTb2, base(4,1)(log(392.1)))

    if (b2%n /= 195+8) error stop 1_4

    do i = 1, 195
        if (.not. precision_r4 (b2%data(i), i+5.0)) error stop 2_4
    end do

    do i = 196, b2%n
        if (.not. precision_r4 (b2%data(i), log(197.0 + i))) error stop 3_4
    end do
end


logical function b1GTb2 (b1, b2)
use m
    class(base(4,1)), intent(in) :: b1, b2

    b1GTb2 = b1%data(1) > b2%data(1)
end function
