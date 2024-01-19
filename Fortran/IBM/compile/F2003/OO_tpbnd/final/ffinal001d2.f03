! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (C1273: Any procedure referenced in a
!                               pure subprogram, including one referenced via a
!                               defined-operation, assignment, or finalization,
!                               shall be pure)
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

    interface assignment (=)
        pure subroutine asgnB2ToB1 (b1, b2)
        import base
            class (base), intent(out) :: b1
            type (base), intent(in) :: b2
        end subroutine
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module


!! there is one reference to finalizeBase for b1, which has INTENT(OUT) attr.
pure subroutine asgnB2ToB1 (b1, b2)
use m, only: base
    class (base), intent(out) :: b1
    type (base), intent(in) :: b2

    b1%id = b2%id
end subroutine


!! there are three refencese to finalizeBase; only two will get caught here
pure subroutine invalid
use m
    type (base) :: b1, b2   !<-- this will generate 2 calls to finalizeBase

    b1 = b2  !<-- since asgnB2ToB1 fails to compile, this will not be caught
end subroutine

program ffinal001d2
    call invalid
end
