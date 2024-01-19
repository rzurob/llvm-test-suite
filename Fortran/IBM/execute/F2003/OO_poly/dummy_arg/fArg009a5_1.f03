! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute used in
!*                               the type-bound)
!*
!*                               2008-01-02: value attribute removed per defect
!*                               324210.1
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20) name

        contains

        procedure :: print => printChild
        final :: finalizeChild
    end type

    contains

    subroutine printBase (b)
        class (base) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeChild (b)
        type(child) :: b

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeBase (b)
        type (base) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg009a5_1
use m
    class (base), allocatable :: b1

    allocate (b1, source=base(100))

    call b1%print

    deallocate (b1)

    allocate (b1, source=child(-100, 'test 1'))

    call b1%print
end
