!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/11/2005
!*
!*  DESCRIPTION                : final sub (finalization of temporaries created
!                               by structure constructor)
!*
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

program ffinal515_1
use m
    type (base) :: b(10) = base (10)   !<-- this should not be finalized

    print *, 'begin'

    b = base (20)    !<-- the temp here should be finalized before next stmt

    print *, 'end'

    if (any(b%id /= 20)) error stop 1_4
end

