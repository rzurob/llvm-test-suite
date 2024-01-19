! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (all temps in an array constructor
!*                               shall be finalized; temps are created by
!*                               structure constructor)
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
    type (base), save :: b(3)

    b = (/(base(i), i=1,3)/)

    print *, 'end'
end

