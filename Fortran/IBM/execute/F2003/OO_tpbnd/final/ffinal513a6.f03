! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of allocatable actual arg
!*                               asssociated with INTENT(OUT) dummy-arg)
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

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine abc (b)
        type (base), allocatable, intent(out) :: b
    end subroutine

    subroutine cba (b)
        type (base), allocatable, intent(out) :: b(:)
    end subroutine
end module

program ffinal513a6
use m
    type (base), allocatable :: b1, b2(:)

    allocate (b1, b2(2))

    call abc (b1)

    print *, 'test array'

    call cba (b2)

    print *, 'end'
end