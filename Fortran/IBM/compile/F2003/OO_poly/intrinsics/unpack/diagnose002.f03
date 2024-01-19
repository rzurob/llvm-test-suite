!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Diagnose test case. MASK shall be of
!                              type logical.
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

program diagnose002
use m
    class(Base), pointer :: b1(:)
    allocate(Base::b1(2))

    print *, unpack(b1, (/1,0/), (/Base(1),Base(2)/))
end
