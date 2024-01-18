!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute used in
!                               the type-bound)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBaseValue (b)
        type(base), value :: b

        print *, b%id
    end subroutine

    subroutine printChildValue (c)
        type(child), value :: c

        print *, c%id, c%name
    end subroutine

    subroutine printBase (b)
        class (base) :: b

        call printBaseValue (b)
    end subroutine

    subroutine printChild (b)
        class (child) :: b

        call printChildValue (b)
    end subroutine
end module

program fArg009a5
use m
    class (base), allocatable :: b1

    allocate (b1, source=base(100))

    call b1%print

    deallocate (b1)

    allocate (b1, source=child(-100, 'test 1'))

    call b1%print
end
