! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (non-polydata involvement)
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
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
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

    function updateVal (b, id)
        class (base), intent(inout), pointer :: b
        class (base), pointer :: updateVal
        integer*4, intent(in) :: id

        b%id = id
        updateVal => b
    end function
end module

program ffuncRet003
use m
    class (base), pointer :: b1

    type (base), pointer :: b2
    type (child), target :: c1

    c1%id = 1
    c1%name = 'c1'

    b1 => c1

    b2 => updateVal (b1, 10)

    call b2%print

    if (associated (b2, b1)) error stop 1_4
end
