!*  ===================================================================
!*
!*  DATE                       : 09/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (when a variable is deallocated, any
!                               allocated allocatable subobject of the variable
!                               is deallocated; pointer component is not
!                               finalizable subobject)
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
        integer(4) :: id

        contains

        final :: finalizeBase
    end type

    type A
        class (base), allocatable :: data

        contains

        final :: finalizeA
    end type

    type B
        type(A), pointer :: data
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeA (a1)
        type (A), intent(in) :: a1

        print *, 'finalizeA'
    end subroutine
end module

program falloc025a2
use m
    type (B), allocatable :: b1

    allocate (b1)
    allocate (b1%data)
    allocate (b1%data%data)

    print *, 'begin'

    deallocate (b1)

    print *, 'end'
end