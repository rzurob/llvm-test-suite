!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/27/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is poly. SHIFT is scalar or array.
!                              DIM is present or not.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program cshift002
use m
    class(Base), pointer :: b1(:,:,:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/3,2,2/)))

    select type(name1=>cshift(b1, -1))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>cshift(b1, 3, 2))
        type is (Child)
            print *, name1
        class default
            error stop 2_4
    end select

    select type(name1=>cshift(b1, -3, 3))
        type is (Child)
            print *, name1
        class default
            error stop 3_4
    end select

    select type(name1=>cshift(b1,reshape((/1,2,-3,-4,5,-6/),(/3,2/)),3))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 4_4
    end select
end
