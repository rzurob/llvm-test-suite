! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc028.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLCOATE (if a pointer is associated with an
!                               allocatable entity, the pointer shall not be
!                               deallocated)
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

    contains

    subroutine finalizeBase (b)
        type (base(4)) b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)) b(*)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program falloc028
use m
    class (*), allocatable, target :: x1_alloc, x2_alloc(:)

    class (*), pointer :: x1_ptr, x2_ptr(:)

    integer(4) err(2)

    allocate (base(4)::x1_alloc, x2_alloc(10))

    x1_ptr => x1_alloc
    x2_ptr => x2_alloc

    err = -1

    deallocate (x1_ptr, stat=err(1))
    deallocate (x2_ptr, stat=err(2))

    if (any (err /= 2)) error stop 1_4
end
