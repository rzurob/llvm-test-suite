! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id = -1

        contains

        procedure :: assignID => assignID2Base
        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    !! this subroutine will be reseting the value of other component in the
    !extended types
    subroutine assignID2Base (b, id)
        class (base), intent(out) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fArg010a3_1
use m
    type (child) :: c1 = child (1, 'c1')

    if (resetTemp (c1%base) /= 1) error stop 1_4

    call c1%print

    contains

    integer(4) function resetTemp (b)
        type (base), VALUE :: b

        resetTemp = b%id

        call b%assignID (100)

        if (b%id /= 100) error stop 10_4
    end function
end
