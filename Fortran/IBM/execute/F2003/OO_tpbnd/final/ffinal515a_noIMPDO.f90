!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/10/2007
!*
!*  DESCRIPTION                : defect 341256: additional test cases for
!                               coverage without AC-IMPDO
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

use m
    type (base), save :: b(3)

    b = (/base(1), base(2), base(3)/)

    print *, 'end'
end

