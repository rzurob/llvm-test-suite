!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2005
!*
!*  DESCRIPTION                : argument association (dummy procedure used as
!                               the defined assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child
        character(20) name
    end type

    type container
        class(base), pointer :: data(:) => null()

        contains

        final :: finalizeContainer
    end type

    interface
        subroutine copyCo1fromCo2 (co1, co2)
        import container
            class(container), intent(out) :: co1
            class(container), intent(in) :: co2
        end subroutine
    end interface

    contains

    subroutine finalizeContainer (co)
        type (container), intent(inout) :: co

        if (associated(co%data)) deallocate (co%data)
    end subroutine

    subroutine copyData (co1, co2, assgn)
        class(container), intent(out) :: co1
        class(container), intent(in) :: co2

        interface assignment(=)
            subroutine assgn (co1, co2)
            import container
                class(container), intent(out) :: co1
                class(container), intent(in) :: co2
            end subroutine
        end interface

        co1 = co2
    end subroutine
end module

program fArg037
use m
    class(container), allocatable :: co1, co2, co3

    procedure (copyCo1fromCo2) assgn1, assgn2

    allocate (co1, co2, co3)
    allocate (co2%data(10), source=child(10, 'test 101'))
    allocate (co1%data(2))

    call copyData (co1, co2, assgn1)

    if (.not. associated (co1%data)) error stop 1_4

    if (size(co1%data) /= 10) error stop 2_4

    select type (x => co1%data)
        type is (child)
            do j = 1, 10
                if ((x(j)%id /= 10) .or. (x(j)%name /= 'test 101')) &
                        error stop 3_4
            end do
        class default
            error stop 4_4
    end select

    call copyData (co1, co3, assgn2)

    if (.not. associated (co1%data)) error stop 5_4

    if (size(co1%data) /= 0) error stop 6_4

    call copyData (co1, co3, assgn1)

    if (associated (co1%data)) error stop 7_4
end


subroutine assgn1 (co1, co2)
use m
    class(container), intent(out) :: co1
    class(container), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data(size(co2%data)), source=co2%data)
    end if
end subroutine


subroutine assgn2 (co1, co2)
use m
    class(container), intent(out) :: co1
    class(container), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data(size(co2%data)), source=co2%data)
    else
        allocate (co1%data(0))
    end if
end subroutine
