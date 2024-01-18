! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (pointer function results are not
!*                               finalized if appears as un-named variables)
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

        procedure :: replicate => replicateBase

        final :: finalizeBase
    end type

    contains

    function replicateBase (b)
        class (base), intent(in) :: b
        class (base), pointer :: replicateBase

        type (base), pointer :: temp

        allocate (temp)
        temp%id = b%id

        replicateBase => temp
    end function

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal522a
use m
    type (base), save :: b1, b2

    b1%id = 100

    b2 = b1%replicate()

    print *, b2%id
end
