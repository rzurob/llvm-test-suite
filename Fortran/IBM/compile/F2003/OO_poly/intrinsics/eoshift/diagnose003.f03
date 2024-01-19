!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/03/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Diagnose test case. DIM shall be scalar.
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

    print *, eoshift(b1, 1, Base(1), (/1/))
end
