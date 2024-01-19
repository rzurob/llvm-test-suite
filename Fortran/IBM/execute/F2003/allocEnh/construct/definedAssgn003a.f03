! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the type-bound defined assignment
!                               would not apply if the defined assignment is not
!                               elemental.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, pointer :: i => null()

        contains

        procedure :: assgnB1B2
        generic :: assignment (=) => assgnB1B2
    end type

    contains

    subroutine assgnB1B2 (b1, b2)
        class(base), intent(inout) :: b1
        type(base), intent(in) :: b2

        if (associated(b1%i)) deallocate (b1%i)

        if (associated(b2%i)) allocate(b1%i, source=-b2%i)
    end subroutine
end module

module m1
use m, only: base
    type container
        type(base) :: data(2)
    end type
end module

program definedAssgn003
use m1
    type (container), allocatable :: co1, co2(:), co3(:)
    integer, target :: i1(2) = (/100, 200/)

    allocate (co3(0:9))

    do i = 0, 9
        allocate(co3(i)%data(1)%i, source = i)
        allocate(co3(i)%data(2)%i, source = i*2)
    end do

    co1 = container((/base(i1(1)), base(i1(2))/))

    co2 = co3

    if ((.not. allocated(co1)) .or. (.not. allocated(co2))) error stop 1_4

    if ((lbound(co2,1) /= 0) .or. (ubound(co2,1) /= 9)) error stop 2_4

    if (.not. associated(co1%data(1)%i, i1(1))) error stop 3_4

    if ((co1%data(1)%i /= 100) .or. (co1%data(2)%i /= 200)) error stop 4_4

    do i = 0, 9
        if ((.not. associated(co2(i)%data(1)%i, co3(i)%data(1)%i)) .or. &
            (.not. associated(co2(i)%data(2)%i, co3(i)%data(2)%i))) &
                error stop 5_4

        if ((co2(i)%data(1)%i /= i) .or. &
            (co2(i)%data(2)%i /= i*2)) error stop 6_4
    end do

    co2 = co3(9:0:-2)

    if ((lbound(co2,1) /= 1) .or. (ubound(co2,1) /= 5)) error stop 7_4

    do i = 1, 5
        if ((.not. associated(co2(i)%data(1)%i, co3(11-2*i)%data(1)%i)) .or. &
            (.not. associated(co2(i)%data(2)%i, co3(11-2*i)%data(2)%i))) &
                error stop 8_4

        if ((co2(i)%data(1)%i /= -2*i+11) .or. (co2(i)%data(2)%i /= -i*4+22)) &
                error stop 9_4
    end do
end
