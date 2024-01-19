! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute and
!                               TARGET used together)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), pointer :: data(:) => null()
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    interface
        subroutine printBase (b)
        import base
            class(base), intent(in), pointer :: b
        end subroutine
    end interface

    contains

    subroutine testBaseValue (b, ptr)
        type(base), target, value :: b
        class(base), pointer :: ptr

        ptr => b

        call printBase (ptr)

        if (associated (ptr%data)) ptr%data = ptr%data + 1.0_8

        call printBase (ptr)
    end subroutine

    subroutine testChildValue (b, ptr)
        type(child), target, value :: b
        class(base), pointer :: ptr

        ptr => b

        call printBase (ptr)

        if (associated (ptr%data)) ptr%data = ptr%data + 1.0_8

        select type (ptr)
            type is (child)
                ptr%name = 'reNamed'
        end select

        call printBase (ptr)
    end subroutine

    subroutine testValue (b)
        class(base), target :: b

        class(base), pointer :: bLocal

        select type (b)
            type is (base)
                call testBaseValue (b, bLocal)

            type is (child)
                call testChildValue (b, bLocal)
        end select
    end subroutine
end module

subroutine printBase (b)
use m, only: base, child
    class(base), intent(in), pointer :: b

    if (.not. associated(b)) return

    select type (b)
        type is (base)
            if (associated(b%data)) write (*, '(1x, 5f10.2)') b%data
        type is (child)
            if (associated(b%data)) then
                write (*, '(1x, 5f10.2)', advance='no') b%data
                write (*, '(1x,a)') b%name
            else
                print *, b%name
            end if
        class default

    end select
end subroutine

program fArg038a1
use m
    class (base), allocatable, target :: b1(:)

    allocate (b1(2), source=(/child(name='test 1'), child(name='test 2')/))

    allocate (b1(1)%data(3), source=(/1.5_8, 2.5_8, 3.5_8/))

    call testValue (b1(1))
    call testValue (b1(1))

    write (*, '(1x, 5f10.2)') b1(1)%data

    call testValue (b1(2))
end
