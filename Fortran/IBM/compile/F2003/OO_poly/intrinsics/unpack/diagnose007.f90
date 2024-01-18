!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/21/2005
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

    type Base1
        integer :: i = 8
        integer :: j = 9
    end type
end module

program diagnose007
use m
    class(Base), pointer :: b1(:)
    class(Base1), allocatable :: f1(:)
    allocate(b1(2), SOURCE=(/Base(-1), Base(-2)/))
    allocate(f1(3), SOURCE=(/Base1(1,2),Base1(3,4),Base1(5,6)/))

    select type(name1=>unpack(b1,(/.TRUE.,.FALSE.,.TRUE./),f1))
        class default
            error stop 1_4
    end select
end
