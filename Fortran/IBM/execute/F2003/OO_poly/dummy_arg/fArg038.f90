!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute and
!                               function results as the actual-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        real(8) value

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class(child), intent(in) :: b

        write (*, '(i10,f12.2)') b%id, b%value
    end subroutine

    subroutine printB1 (b)
        type(base), value :: b

        call printB (b)
    end subroutine

    subroutine printC1 (b)
        type(child), value :: b

        call printB (b)
    end subroutine

    recursive subroutine printB (b)
        class (base) :: b

        logical :: pass = .false.

        if (.not. pass) then
            pass = .true.

            select type (b)
                type is (child)
                    call printC1 (b)

                class default
                    call printB1(b)
            end select
        else
            pass = .false.

            call b%print
        end if

    end subroutine

    class(base) function makeData (id, value)
        integer, value :: id
        real(8), optional, value :: value
        allocatable makeData

        if (present(value)) then
            allocate (makeData, source=child(id, value))
        else
            allocate (makeData, source=base(id))
        end if
    end function

    type (base) function makeBaseObj (id)
        integer, value :: id

        makeBaseObj%id = id
    end function

    type (child) function makeChildObj (id, value)
        integer, value :: id
        real(8), value :: value

        makeChildObj%id = id
        makeChildObj%value = value
    end function
end module

program fArg038
use m
    !! structure constructor
    call printB (base(10))
    call printB (child(100, 1.5_8))

    !! poly-function results
    call printB (makeData (20))
    call printB (makeData (200, 3.1_8))

    !! non-poly function result
    call printB (makeBaseObj (300))
    call printB (makeChildObj (30, 5.2_8))
end
