!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Diagnose test case. MASK shall be
!                              conformable with ARRAY.
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

program diagnose003
use m
    type(Base) :: b1(3)
    b1%i = (/-1,-2,-3/)

    print *, pack(b1, (/.TRUE.,.FALSE./))
end
