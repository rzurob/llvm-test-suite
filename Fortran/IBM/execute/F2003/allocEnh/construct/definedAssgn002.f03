! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that intrinsic assignment for an
!                               allocatable of a derived type with allocatable
!                               component that is of another derived type with
!                               type-bound defined assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, pointer :: id => null()

        contains

        procedure :: assgn => assignB1B2
        generic :: assignment(=) => assgn
    end type

    type, extends(base) :: child
        character(:), pointer :: name => null()

        contains

        procedure :: assgn => assignB1B2Child
    end type

    contains

    subroutine assignB1B2 (b1, b2)
        class(base), intent(inout) :: b1
        class(base), intent(in) :: b2

        if (associated(b1%id)) deallocate (b1%id)

        if (associated(b2%id)) allocate(b1%id, source=-b2%id)
    end subroutine

    subroutine assignB1B2Child (b1, b2)
        class(child), intent(inout) :: b1
        class(base), intent(in) :: b2

        select type (b2)
            type is (child)
                !! first call the base type's assignment to assign the base
                !value
                b1%base = b2%base

                !! then do a deep copy of name
                if (associated(b1%name)) deallocate(b1%name)

                if (associated(b2%name)) allocate (b1%name, source=b2%name)

            class default
                stop 10
        end select
    end subroutine
end module


module m1
use m, only: base
    type container
        class(base), allocatable :: data
    end type
end module


program definedAssgn002
use m1
use m, only: child
    type(container), allocatable :: co1, co2, co3(:), co4(:)

    integer, target :: id1 = 100
    character(20), target :: name1 = 'xlftest 101'
    integer, pointer :: id
    character(:), pointer :: name

    co1 = container(base(id1))

    co2 = container(data=child(id1, name1))

    allocate(co4(0:9))

    do i = 0, 9, 2
        !! note for the source-expr in the allocate statements, no
        !assignment occurs
        allocate (id, source=i)
        allocate (name, source=repeat(achar(65+i),i))

        allocate (co4(i)%data, source=base (id))

        allocate (id, source = i+1)

        allocate (co4(i+1)%data, source=child(id, name))
    end do

    co3 = co4

    !! verify co1 and co2
    if ((.not. allocated(co1)) .or. (.not. allocated(co2)) &
        .or. (.not. allocated(co3))) error stop 1_4

    if ((.not. same_type_as(co1%data, base())) .or. &
        (.not. same_type_as(co2%data, child())))  error stop 2_4

    if ((.not. associated(co1%data%id)) .or. &
        associated(co1%data%id, id1)) error stop 3_4

    if (co1%data%id /= -100) error stop 4_4

    deallocate(co1)

    if ((.not. associated(co2%data%id)) .or. &
        associated(co2%data%id, id1)) error stop 5_4

    if (co2%data%id /= -100) error stop 6_4

    select type (x => co2%data)
        type is (child)
            if ((.not. associated(x%name)) .or. &
                associated(x%name, name1)) error stop 7_4

            if (x%name%len /= 20) error stop 8_4

            if(x%name /= name1) error stop 9_4

        class default
            stop 20
    end select

    deallocate (co2, co4)

    !! verify co3
    if (.not. allocated(co3)) error stop 10_4

    if ((lbound(co3, 1) /= 0) .or. (ubound(co3,1) /= 9)) error stop 11_4

    do i = 0, 9, 2
        if ((.not. same_type_as(co3(i)%data, base())) .or. &
            (.not. same_type_as(co3(i+1)%data, child()))) error stop 12_4

        if ((co3(i)%data%id /= -i) .or. (co3(i+1)%data%id /= -i-1)) &
            error stop 13_4

        select type (x => co3(i+1)%data)
            class is (child)
                if (.not. associated(x%name)) error stop 14_4
                if (x%name%len /= i) error stop 15_4

                if (x%name /= repeat(achar(65+i), i)) error stop 16_4

            class default
                stop 30
        end select
    end do
end
