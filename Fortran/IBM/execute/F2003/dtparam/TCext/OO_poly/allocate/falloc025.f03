! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc025.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (when a variable of derived type is
!                               deallocated, any allocated allocatable subobject
!                               is deallocated)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        final :: finalizeBase
    end type

    type A(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: b1
        type(base(k2)), allocatable  :: b2
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program falloc025
use m
    type (A(4)), allocatable :: a1
    class (A(4)), allocatable :: a2

    allocate (a1, a2)

    allocate (a1%b1, a1%b2, a2%b1, a2%b2)

    print *, 'deallocating a1'

    deallocate (a1)

    print *, 'deallocating a2'

    deallocate (a2)

    print *, 'end'
end