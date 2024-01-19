! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (optinal INTENT(OUT) dummy-arg)
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
        integer*4 :: data

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

    end subroutine
end module

program ffinal512
use m
    interface
        subroutine abc (b)
        use m
            type (base), optional, intent(out) :: b
        end subroutine
    end interface

    type (base) :: b1

    call abc (b1)

    call abc

    print *, 'end of program'
end

subroutine abc (b)
use m
    type (base), optional, intent(out) :: b

    if (present (b)) print *, 'you should see finalizeBase by now'
end subroutine
