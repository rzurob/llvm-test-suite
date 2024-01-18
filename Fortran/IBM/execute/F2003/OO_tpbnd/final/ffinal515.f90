! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temporaries created
!*                               by structure constructor; at assignment)
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

use m
    type (base) :: b = base (10)   !<-- this should not be finalized

    print *, 'begin'

    b = base (20)    !<-- the temp here should be finalized before next stmt

    print *, 'end'
end

