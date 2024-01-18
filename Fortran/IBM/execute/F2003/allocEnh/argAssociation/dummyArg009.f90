!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the defined type-bound assignment
!                               applied to the allocatable dummy-arg's component
!                               which is nonpointer, nonallocatable.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data
        integer, pointer :: id

        contains

        procedure, private :: uncommonAssgn
        generic :: assignment(=) => uncommonAssgn
    end type

    private uncommonAssgn
    contains

    !! this is rather unusual way to do assignment: choose the larger value
    !between LHS and RHS for components
    subroutine uncommonAssgn (b1, b2)
        class(base), intent(inout) :: b1
        class(base), intent(in) :: b2

        real, allocatable :: local
        integer, pointer :: localInt

        call move_alloc(b1%data, local)
        localInt => b1%id

        if (allocated(b2%data)) then
            b1%data = b2%data

            if (allocated(local)) then
                local = max(b1%data, local)
            else
                local = b2%data
            end if
        end if

        call move_alloc(local, b1%data)

        if (associated(b2%id)) then
            allocate(b1%id)

            if (associated(localInt)) then
                b1%id = max(b2%id, localInt)

                deallocate(localInt)
            else
                b1%id = b2%id
            end if

        end if
    end subroutine
end module

module m1
use m
    type container
        type(base) data
    end type

    contains

    subroutine assgnCo (co1, co2)
        type(container), allocatable :: co1
        type(container) co2

        co1 = co2
    end subroutine
end module

program dummyArg009
use m1
    type(container), allocatable :: co1, co2, co3
    type (container) co4

    integer, target :: it

    logical(4), external :: precision_r4


    allocate (co1,co2)

    co1%data%data = 10.1
    co4%data%data = 20.2

    allocate (co1%data%id, co4%data%id)

    co1%data%id = 10
    co4%data%id = 1

    !! test 1
    call assgnCo (co1, co4)

    if (.not. precision_r4(co1%data%data, 20.2_4)) error stop 1_4

    if (co1%data%id /= 10) error stop 2_4

    nullify (co2%data%id)

    !! test 2
    call assgnCo (co1, co2)
    call assgnCo (co2, co1)

    if (.not. precision_r4(co1%data%data, 20.2_4)) error stop 3_4

    if (co1%data%id /= 10) error stop 4_4

    if (.not. precision_r4(co2%data%data, 20.2_4)) error stop 5_4

    if (co2%data%id /= 10) error stop 6_4

    if (associated(co1%data%id, co2%data%id)) error stop 7_4

    !! test 3
    it = 100
    call assgnCo (co1, container(base(10, it)))
    call assgnCo (co3, co1)

    if (.not. precision_r4(co3%data%data, 20.2_4)) error stop 8_4
    if (.not. precision_r4(co1%data%data, 20.2_4)) error stop 9_4

    if ((co3%data%id /= 100) .or. (co1%data%id /= 100)) error stop 10_4

    if (associated(co1%data%id, it) .or. &
        associated(co1%data%id, co3%data%id)) error stop 11_4
end
