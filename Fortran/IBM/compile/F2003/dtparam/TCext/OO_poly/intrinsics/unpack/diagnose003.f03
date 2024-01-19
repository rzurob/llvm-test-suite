! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/intrinsics/unpack/diagnose003.f
! opt variations: -qnol -qnodeferredlp

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Diagnose test case. MASK shall be
!                              an VECTOR.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 9
    end type
end module

program diagnose003
use m
    class(Base(:,4)), allocatable :: b1(:)
    allocate(Base(20,4)::b1(3))

    print *, unpack(b1, .TRUE., Base(20,4)(1))
end
