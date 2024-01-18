!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
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

program diagnose005
use m
    type(Base) :: b1(1)
    b1%i = (/-1/)

    print *, pack(b1, .TRUE., reshape((/Base(1),Base(2)/),(/1,1/)))
end
