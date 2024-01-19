!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Diagnose test case. ARRAY shall not be
!                              scalar.
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

    print *, pack(b1, .TRUE., (/Base(1),Base(2)/))
end
