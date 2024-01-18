! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (function return result is finalized
!*                               after the use in defined assignment)
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
        integer*4, pointer :: id => null()

        contains

        procedure :: replicate => produceBase

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%id)) then
            print *, 'deallocating data'
            deallocate (b%id)
        end if
    end subroutine

    type (base) function produceBase (b)
        class (base), intent(in) :: b

        allocate (produceBase%id)

        produceBase%id = b%id
    end function
end module

program ffinal514a
use m
    interface assignment (=)
        subroutine base2Base (b1, b2)
        use m
            type (base), intent(out) :: b1
            type (base), intent(in) :: b2

        end subroutine
    end interface

    type (base), save :: b1, b2

    allocate (b1%id)

    b1%id = 100

    b2 = b1%replicate()

    print *, 'end'
end

subroutine base2Base (b1, b2)
use m
    type (base), intent(out) :: b1
    type (base), intent(in) :: b2

    if (associated (b2%id)) then
        allocate (b1%id)

        b1%id = b2%id
    end if
end subroutine
