! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/null/argAssociation008.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/04/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : null is an actual argument. Dummy
!                              argument is with assumed character length.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program argAssociation008
use m
    class(*), pointer :: c1

    allocate(character(8)::c1)

    call sub1(null(c1))

    contains

    subroutine sub1(arg1)
        class(*), pointer :: arg1

        if(associated(arg1)) error stop 1_4
    end subroutine
end
