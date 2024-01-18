
module m
    type base
        integer id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base), intent(inout) :: b

        print *, b%id

        b%id = b%id + 1
    end subroutine

    subroutine printChild (b)
        class (child), intent(inout) :: b

        call b%base%print

        print *, b%name
    end subroutine

    subroutine test1 (b)
        class(base), intent(inout) :: b(3)

        do i = 1, 3
            associate (x => b(i))
                call x%print
            end associate
        end do
    end subroutine
end module

use m

    class (base), allocatable :: b(:)

    allocate (b(10), source=(/(child(i, 'xlftest'), i=1,10)/))

    call ttt (b)

    print *, b%id

    contains

    subroutine ttt (x)
        class(base), intent(inout) :: x(*)

        call test1 (x)
    end subroutine
end
