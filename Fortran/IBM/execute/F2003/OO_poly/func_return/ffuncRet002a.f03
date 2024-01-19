! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (poly-data's vft)
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

        procedure :: print => printBase
        final :: finalizeBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
        final :: finalizeChild
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

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    function replicateBase (b)
        class (base), intent(in) :: b
        class (base), pointer :: replicateBase

        allocate (replicateBase, source=b)
    end function
end module

program ffuncRet002a
use m
    class (base), pointer :: b1

    b1 => replicateBase (child (1, 'b1_pointer'))

    call b1%print

    deallocate (b1)
end
