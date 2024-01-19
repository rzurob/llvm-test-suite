! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (temp created by structure
!*                               constructor finalized; in ASSOCIATE construct)
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
        integer*4 :: id

        contains

        final :: finalizeBase
        procedure, pass(b1) :: diff => IDdiff
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    integer*4 function IDdiff (b1, b2)
        type (base), intent(in) :: b2
        class (base), intent(in) :: b1

        IDdiff = (b1%id - b2%id)
    end function
end module

program ffinal515a9
use m
    type (base) :: b1 = base(10)

    associate (x => b1%diff (base(6)))
        print *, x
    end associate

    print *, 'end'
end
