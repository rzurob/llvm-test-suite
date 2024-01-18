! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2005
!*
!*  DESCRIPTION                : poly function return (used as the actual arg to
!                               be associated with intent(in) dummy-pointer-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), allocatable :: ids(:)

        contains

        procedure :: print => printBase
        procedure :: makeData => makeBaseAlloc
    end type

    type, extends(base) :: child
        character(20), pointer :: names(:) => null()

        contains

        procedure :: print => printChild
        procedure, pass(c) :: assgnName => allocateNames
        procedure :: makeData => makeChildAlloc
        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        if (associated (c%names)) then
            print *, 'deallocating names'

            deallocate (c%names)
        end if
    end subroutine

    subroutine allocateNames (names, c)
        class (child), intent(inout) :: c
        character(*), intent(in) :: names(:)

        if (associated (c%names)) deallocate (c%names)

        allocate (c%names(size(names)), source=names)
    end subroutine

    subroutine printBase (b)
        class(base), intent(in) :: b

        if (allocated (b%ids)) then
            print *, b%ids
        end if
    end subroutine

    subroutine printChild (b)
        class(child), intent(in) :: b

        call b%base%print

        if (associated(b%names)) then
            print *, b%names
        end if
    end subroutine

    class (base) function makeBaseAlloc (b)
        allocatable makeBaseAlloc
        class (base), intent(in) :: b

        allocate (makeBaseAlloc, source=b)
    end function

    class (base) function makeChildAlloc (b)
        allocatable makeChildAlloc
        class (child), intent(in) :: b

        allocate (makeChildAlloc, source=child(b%ids))

        select type (makeChildAlloc)
            type is (child)
                call makeChildAlloc%assgnName(b%names)
            class default
                error stop 10_4
        end select
    end function

    subroutine printData (d)
        class (base), allocatable, intent(in) :: d

        call d%print
    end subroutine
end module

program ffuncRet015
use m
    class (base), pointer :: b1
    class (base), allocatable :: b2

    allocate (b1, source=child((/5,4,3,2,1/)))

    select type (b1)
        type is (child)
            call b1%assgnName ((/'test 5', 'test 4', 'test 3', 'test 2', &
                        'test 1'/))
        class default
            error stop 1_4
    end select

    allocate (b2, source=b1%makeData())
    if (allocated (b2)) then
        call printData (b2)
    end if

    deallocate (b1)

    print *, 'end'
end
