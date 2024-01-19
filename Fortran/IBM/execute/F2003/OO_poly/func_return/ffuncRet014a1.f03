! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2005
!*
!*  DESCRIPTION                : poly function results (global pointer involved
!                               in program)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    class(base), pointer, private :: gPtr(:) => null()

    contains

    class (base) function makeData (data, names)
        pointer makeData(:)
        real(8), intent(in) :: data(:)
        character(*), intent(in), optional :: names(size(data))

        if (present(names)) then
            allocate (makeData(0:size(data)-1), source=(/(child(data(i), &
                        names(i)), i = 1, size(data))/))
        else
            allocate (makeData(0:size(data)-1), source=(/(base(data(i)), &
                        i = 1, size(data))/))
        end if
    end function

    subroutine updateGlobal (b)
        class (base), intent(in), pointer :: b(:)

        if (associated(gPtr)) then
            deallocate (gPtr)
        end if

        gPtr => b
    end subroutine

    subroutine printGlobal ()
        if (associated (gPtr)) then
            do i = lbound(gPtr,1), ubound(gPtr,1)
                call gPtr(i)%print
            end do
        end if
    end subroutine

    subroutine printBase (b)
        class(base), intent(in) :: b

        if (allocated (b%data)) then
            write (*, '(f10.2)') b%data
        end if
    end subroutine

    subroutine printChild (b)
        class(child), intent(in) :: b

        call b%base%print

        print *, b%name
    end subroutine
end module

program ffuncRet014a1
use m
    real(8) r1(5)
    character(20) names(5)

    r1 = (/(j*1.5_8, j = 1, 5)/)
    names = (/'test 01', 'test 02', 'test 03', 'test 04', 'test 05'/)

    call updateGlobal (makeData (r1, names))

    call printGlobal

    !! second test
    call updateGlobal (makeData (r1))

    call printGlobal
end
