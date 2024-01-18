! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (unlimited poly entities'
!                               finalization)
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

    type, extends(base) :: child
        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child) :: c(:)

        print *, 'finalizeChildRank1 and then', size (c), 'base parent comp.'
    end subroutine
end module

program ffinal014a3
use m
    class (*), pointer :: x1, x2(:)
    class (*), allocatable :: x3, x4(:)

    allocate (child :: x1, x3, x2(2), x4(3))

    print *, 'deallocating x1, x3'

    deallocate (x1, x3)

    print *, 'deallocating x2'

    deallocate (x2)

    print *, 'deallocating x4'

    deallocate (x4)

    print *, 'end'
end
