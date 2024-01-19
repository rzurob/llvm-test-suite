! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Correct test case for dtparamConstr016d.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data
        integer(k) :: id (n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
    end type

    type (child(8,:,:)), allocatable :: c1
end module

program dtparamConstr016
use m
    class(base(4,:)), pointer :: b1

    logical(4), external :: precision_r4, precision_r8

    integer(8) temp

    allocate (b1, source=child(4, 20, 30)(data=1.0, id=(/(i, i=1,20)/), &
            name='b1'))

    allocate (child(8, 55, 25) :: c1)

    c1 = child(8, 55, 25)(data=2.0d0, id=(/(2_8**i, i=1,55)/), &
            name='c1')

    !! verify
    if (c1%name /= 'c1') error stop 1_4

    if (.not. precision_r8(c1%data, 2.0d0)) error stop 2_4
    if (.not. precision_r4(b1%data, 1.0)) error stop 3_4

    do i = 1, 20
        if (b1%id(i) /= i) error stop 4_4
    end do

    temp = 1

    do i = 1, 55
        temp = temp * 2

        if (c1%id(i) /= temp) error stop 5_4
    end do

    select type (b1)
        type is (child(4,*,*))
            if(b1%name /= 'b1') error stop 6_4

        class default
            error stop 7_4
    end select
end
