! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/20/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Unlimited poly allocatable components in
!                               the parameterized derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        class(*), allocatable :: data(:)
    end type

    type, extends(base) :: child
        integer(k) id
    end type
end module

program dtparamExtends033
use m
    class(base(8)), allocatable :: b1
    class(base(4)), allocatable :: b2(:)

    allocate (b1)
    allocate (child(4) :: b2(10))

    allocate (child(8):: b2(8)%data(2), b2(9)%data(10), b2(10)%data(1000))

    if (allocated (b1%data)) error stop 1_4

    do i = 1, 7
        if (allocated (b2(i)%data)) error stop 2_4
    end do

    do i = 8, 10
        if (.not. allocated(b2(i)%data)) error stop 3_4
    end do

    if (same_type_as(b1, b2)) error stop 4_4

    !! type parameter is NOT a factor in determining type
    if (.not. same_type_as (b2, b2(10)%data))   error stop 5_4
end
