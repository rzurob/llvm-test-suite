! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Generic name and structure constructor
!                               collision: correct reference to structure
!                               constructor in this situation
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = -1.0
    end type

    interface base
        module procedure genBase4_1
        module procedure genBase4_2
    end interface

    contains

    function genBase4_1 (val)
        real(4), intent(in) :: val(:)

        type(base(4, size(val))) genBase4_1

        genBase4_1%data = val
    end function

    function genBase4_2 (i, val)
        integer, intent(in) :: i
        real(4), intent(in) :: val(i)

        type(base(4,i)) genBase4_2

        genBase4_2%data = val
    end function
end module

program dtparamConstr019
use m
    type (base(4, 25)) b1

    type(base(4, :)), allocatable :: b2, b3

    logical(4), external :: precision_r4

    b1 = base(4,25)(data=(/(i*1.0,i=1,25)/))

    allocate (base(4,33) :: b2)

    allocate (base(4,200) :: b3)

    b2 = base(33, (/(i+1.2e0, i=1,33)/))

    b3 = base(4, 200)()

    !! verify results
    do i = 1, 25
        if (.not. precision_r4(b1%data(i), i*1.0)) error stop 1_4
    end do

    do i = 1, 33
        if (.not. precision_r4(b2%data(i), i+1.2e0)) error stop 2_4
    end do

    do i = 1, 200
        if (.not. precision_r4(b3%data(i), -1.0)) error stop 3_4
    end do
end
