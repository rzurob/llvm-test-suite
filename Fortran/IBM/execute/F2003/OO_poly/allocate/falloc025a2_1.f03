! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2005
!*
!*  DESCRIPTION                : allocate (auto-deallocation for allocatable
!                               subobjects does not apply to pointers'
!                               components)
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
        integer id

        contains

        final :: finalizeBase
    end type

    type A
        type(base), allocatable :: b1
    end type

    type B
        type (A), pointer :: a1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

subroutine xyz
use m
    type (B) :: b1

    allocate (b1%a1)
    allocate (b1%a1%b1)
end subroutine


program falloc025a2_1
    print *, 'begin'
    call xyz

    print *, 'end'
end
