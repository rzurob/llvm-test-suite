! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Put entities with allocatable component in
!                               the array constructor; use type-spec in the
!                               array constructor; test deep copy behavior.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l)
        integer, kind :: k
        integer, len :: l

        real(k), allocatable :: data(:)
        character(l) :: name
    end type
end module

program dtparamArrConstr002
use m
    type (base(4,:)), allocatable :: b1, b2, b3, b4(:)
    logical(4), external :: precision_r4

    allocate (base(4, 20) :: b1, b2, b3, b4(3))

    allocate (b1%data(-1:1), source=(/(i*1.0, i=-1, 1)/))
    allocate (b2%data(0:1), source=(/0.0, 1.0/))
    allocate (b3%data(1), source=1.0)

    b1%name = 'element b1'
    b2%name = 'element b2'
    b3%name = 'element b3'

    b4 = (/base(4, 20) :: b1, b2, b3/)

    !! verify the results
    if (any(b4%name /= (/'element b1', 'element b2', 'element b3'/))) &
            error stop 1_4

    if ((size(b4(1)%data) /= 3) .or. (size(b4(2)%data) /= 2) .or. &
        (size(b4(3)%data) /= 1)) error stop 2_4

    if ((.not. precision_r4(b4(1)%data(-1), -1.0)) .or. &
        (.not. precision_r4(b4(1)%data(0), 0.0)) .or.   &
        (.not. precision_r4(b4(1)%data(1), 1.0))) error stop 3_4


    if ((.not. precision_r4(b4(2)%data(0), 0.0)) .or.   &
        (.not. precision_r4(b4(2)%data(1), 1.0))) error stop 4_4

    if (.not. precision_r4(b4(3)%data(1), 1.0)) error stop 5_4
end
