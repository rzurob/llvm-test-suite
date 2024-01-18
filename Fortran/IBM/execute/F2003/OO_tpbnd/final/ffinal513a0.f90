! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (very basic test for intent(out)
!*                               finalization)
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
    end type

    type (base) :: b1_m(2)

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal513a0
use m
    call abc (b1_m(1))

    call cba (b1_m(2))

    print *, b1_m
    contains

    subroutine abc (b)
        type (base), intent(out) :: b

        print *, 'abc'

        b%id = 1
    end subroutine

    subroutine cba (b)
        class (base), intent(out) :: b

        print *, 'cba'

        b%id = 2
    end subroutine
end
