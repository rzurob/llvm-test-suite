!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Diagnose test case. FIELD shall be of
!                              the same type and type parameters as
!                              VECTOR.
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

program diagnose004
use m
    type(Base) :: b1(2)
    b1%i = (/-1,-2/)

    print *, unpack(b1, (/.TRUE.,.FALSE./), (/1,2/))
end
