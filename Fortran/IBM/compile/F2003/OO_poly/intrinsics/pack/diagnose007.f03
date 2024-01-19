!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Diagnose test case. VECTOR shall be of
!                              the same type and type parameters as
!                              ARRAY.
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

    type Base1
        integer :: i = 8
        integer :: j = 9
    end type
end module

program diagnose007
use m
    type(Base) :: b1(2)
    b1%i = (/-1,-2/)

    print *, pack(b1,.TRUE.,(/Base1(1,2),Base1(3,4),Base1(5,6)/))
end
