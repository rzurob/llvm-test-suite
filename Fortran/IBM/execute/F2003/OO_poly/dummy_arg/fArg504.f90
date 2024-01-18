!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2005
!*
!*  DESCRIPTION                : argument association (miscellaneous)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id = 0

        contains

        procedure :: print => printBase
        procedure, nopass :: printType => printBaseType
    end type

    type, extends(base) :: child
        character(20) :: name = ''

        contains

        procedure :: print => printChild
        procedure, nopass :: printType => printChildType
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printChildType (i)
        integer, intent(out) :: i

        i = 2
    end subroutine

    subroutine printBaseType (i)
        integer, intent(out) :: i

        i = 1
    end subroutine
end module

program fArg504
use m
    type (child) :: c1 = child(10, 'c1')

    call abc (c1)

    contains

    subroutine abc (cc)
        IMPLICIT class (base) (c)

        call c1%print

        call cc%print
        call cc%printType (i1)

        if (i1 /= 2) error stop 1_4
    end subroutine
end
