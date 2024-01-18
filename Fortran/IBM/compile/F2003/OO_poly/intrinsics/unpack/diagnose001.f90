!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Diagnose test case. VECTOR shall have
!                              rank one.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 9
    end type
end module

program diagnose001
use m
    type(Base) :: b1

    print *, unpack(b1, (/.TRUE.,.FALSE./), (/Base(1),Base(2)/))
end
