! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc025a1.f
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
!                               of the variable is deallocated)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    type A(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data1(:)
        type(base(k2)), allocatable  :: data2(:)
    end type

    contains

    subroutine finalizeBase (b)
        type(base(4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program falloc025a1
use m
    type(A(4)), allocatable :: a1
    class (A(4)), pointer :: a2(:)

    allocate (a1, a2(1:3))

    allocate (a1%data1(2), a1%data2(3))

    print *, 'deallocate a1'

    deallocate (a1)

    allocate (a2(1)%data2(100), a2(2)%data1(2), a2(2)%data2(10), a2(3)%data1(20))

    print *, 'deallocate a2'

    deallocate (a2)

    print *, 'end'
end