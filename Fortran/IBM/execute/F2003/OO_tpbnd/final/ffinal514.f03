! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (function return result is finalized
!*                               after the use in assignment)
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

        procedure :: replicate => produceBase

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    type (base) function produceBase (b)
        class (base), intent(in) :: b

        produceBase%id = b%id
    end function
end module

program ffinal514
use m
    type (base), save :: b1

    b1%id = 100

    b1 = b1%replicate()

    print *, 'end'
end