! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/08/2005
!*
!*  DESCRIPTION                : final subroutine (finalization of rank-one
!                               array component)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: id = 1

        contains

        final :: finalizeBase, finalizeBaseArray1
    end type

    type container
        type (base) :: data(2)
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseArray1'
    end subroutine
end module

program ffinal006
use m
    class (container), allocatable :: co1(:)

    allocate (co1(2))

    deallocate (co1)

    print *, 'end'
end
