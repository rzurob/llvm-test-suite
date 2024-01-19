! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final (static variables not finalized)
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
        integer*4, pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase(b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

program ffinal500
use m
    type (base), save:: b1

end
