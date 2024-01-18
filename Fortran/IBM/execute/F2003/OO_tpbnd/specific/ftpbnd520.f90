! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type-bound (binding needs to be
!*                               accessible in ASSOCIATE construct)
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
        integer*4 :: id = 1

        contains

        procedure :: print => printBase
        procedure :: addID => addID2Base
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addID2Base (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine
end module

program ftpbnd520
use m
    type (base) :: b1

    associate (x => b1)
        call x%addID (9)

        call x%print
    end associate

    call b1%print
end
