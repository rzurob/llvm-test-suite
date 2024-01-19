! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (elemental finalizer)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    elemental subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        b%id = -1
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal527a
use m
    class (base), allocatable :: b1(:,:)

    allocate (b1(3,3))

    call abc (b1)

    if (allocated (b1)) error stop 1_4

    contains

    subroutine abc (b)
        class (base), intent(out), allocatable :: b(:, :)

        if (allocated (b)) error stop 1_4
    end subroutine
end
