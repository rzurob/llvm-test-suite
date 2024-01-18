!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/14/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case where a component of a structure
!                               is of a derived type with type-bound assignment.
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

    elemental subroutine assgnB1B2 (b1, b2)
        class(base), intent(inout) :: b1
        type(base), intent(in) :: b2

        if (associated(b1%i)) deallocate (b1%i)

        if (associated(b2%i)) allocate(b1%i, source=b2%i)
    end subroutine
end module

module m1
use m, only: base

    type container
        type(base) :: data
    end type
end module

program definedAssgn001
use m1
    type (container), allocatable :: co1, co2(:), co3(:)
    integer, target :: i1 = 100

    allocate (co3(0:9))

    do i = 0, 9
        allocate(co3(i)%data%i, source = i)
    end do

    co1 = container(base(i1))

    co2 = co3

    if ((.not. allocated(co1)) .or. (.not. allocated(co2))) error stop 1_4

    if ((lbound(co2,1) /= 0) .or. (ubound(co2,1) /= 9)) error stop 2_4

    if ((.not. associated(co1%data%i)) .or. associated(co1%data%i, i1)) &
            error stop 3_4

    if (co1%data%i /= 100) error stop 4_4

    do i = 0, 9
        if ((.not. associated(co2(i)%data%i)) .or. &
            associated(co2(i)%data%i, co3(i)%data%i)) error stop 5_4

        if (co2(i)%data%i /= i) error stop 6_4
    end do

    co2 = co3(9:0:-2)

    if ((lbound(co2,1) /= 1) .or. (ubound(co2,1) /= 5)) error stop 7_4

    do i = 1, 5
        if ((.not. associated(co2(i)%data%i)) .or. &
            associated(co2(i)%data%i, co3(11-2*i)%data%i)) error stop 8_4

        if (co2(i)%data%i /= co3(11-2*i)%data%i) error stop 9_4
    end do
end
