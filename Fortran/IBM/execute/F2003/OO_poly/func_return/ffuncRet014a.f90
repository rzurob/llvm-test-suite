!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2005
!*
!*  DESCRIPTION                : poly-function return (poly-function return is a
!                               global pointer)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id

        contains

        procedure :: print => printBase
        procedure, non_overridable :: makeData => updateNGetGPtr
    end type

    type, extends(base) :: child
        character(20) name

        contains

        procedure :: print => printChild
    end type

    class (base), pointer, private :: gPtr => null()

    contains

    subroutine printGlobal ()
        if (associated(gPtr)) then
            call gPtr%print
        else
            print *, 'pointer not associated'
        end if
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine updateVal (b, id, name)
        class (base), intent(in), pointer :: b
        integer(8), intent(in) :: id
        character(*), intent(in) :: name

        if (associated (b)) then
            select type (b)
                class is (base)
                    b%id = id
                class is (child)
                    b%id = id
                    b%name = name
                class default
                    error stop 10_4
            end select
        end if
    end subroutine

    class (base) function updateNGetGPtr (b)
        pointer updateNGetGPtr
        class (base), intent(in) :: b

        if (associated(gPtr)) deallocate (gPtr)

        allocate (gPtr, source=b)

        updateNGetGPtr => gPtr
    end function
end module

program ffuncRet014a
use m
    class (base), allocatable :: b1

    allocate (b1, source=child(100_8, 'xlftest team'))

    call updateVal (b1%makeData(), -100_8, 'test xlf')

    call printGlobal

    deallocate (b1)

    allocate (b1, source=base(2000))

    call updateVal (b1%makeData(), 200_8, 'test')

    call printGlobal
end
