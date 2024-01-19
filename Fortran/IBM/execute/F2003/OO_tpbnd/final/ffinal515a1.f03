! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (temp var created by structure
!*                               constructor in procedure calls)
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
        integer*4 id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) ::b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal515a1
use m
    interface
        subroutine abc (b)
        use m
            type (base), intent(in) :: b
        end subroutine
    end interface

    call abc (base(1))

    print *, 'end'
end

subroutine abc (b)
use m
    type (base), intent(in) :: b

    print *, b%id
end subroutine
