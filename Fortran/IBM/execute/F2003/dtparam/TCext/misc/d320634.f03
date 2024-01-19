! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/misc/d320634.f
! opt variations: -qck -ql

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base(4)), intent(inout) :: b

        print *, b%id

        b%id = b%id + 1
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(inout) :: b

        call b%base%print

        print *, b%name
    end subroutine

    subroutine test1 (b)
        class(base(4)), intent(inout) :: b(3)

        do i = 1, 3
            associate (x => b(i))
                call x%print
            end associate
        end do
    end subroutine
end module

use m

    class (base(4)), allocatable :: b(:)

    allocate (b(10), source=(/(child(4,20)(i, 'xlftest'), i=1,10)/))

    call ttt (b)

    print *, b%id

    contains

    subroutine ttt (x)
        class(base(4)), intent(inout) :: x(*)

        call test1 (x)
    end subroutine
end
