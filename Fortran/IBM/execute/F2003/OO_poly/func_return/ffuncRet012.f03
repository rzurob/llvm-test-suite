! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2005
!*
!*  DESCRIPTION                : poly function return (use-rename of the type)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base

        contains

        procedure(printBase), pass(b), deferred :: print
    end type

    type, extends(base) :: child
        integer id
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine printBase (b)
        import base
            class(base), intent(in) :: b
        end subroutine
    end interface

    contains

    class(base) function produceBasePtr (id, name)
        pointer produceBasePtr
        integer, intent(in) :: id
        character(*), intent(in) :: name

        allocate (produceBasePtr, source=child(id,name))
    end function

    subroutine printChild (b)
        class(child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program ffuncRet012
use m, absType => base
    class (absType), pointer :: a1

    a1 => produceBasePtr (100, 'xlf test')

    call a1%print
end
