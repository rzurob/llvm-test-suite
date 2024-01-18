!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/8/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 327897)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: id

        contains

        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBaseRank1 (b1)
        type(base), intent(inout) :: b1(:)

        print *, 'finalizeBaseRank1'

        print *, lbound(b1,1), ubound(b1,1)
    end subroutine
end module

use m
    type(base) :: b1(100)

    b1%id = [(i, i=1, 100)]

    b1(:) = b1(:)

    if (any(b1%id /= [(j, j=1,100)])) stop 1
end
