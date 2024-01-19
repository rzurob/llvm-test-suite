!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is unlimited poly. SHIFT is scalar
!                              or array. BOUNDARY is scalar or array, non
!                              poly or poly. DIM is present or not.
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

program eoshift003
use m
    class(*), pointer :: b1(:,:,:)
    class(*), allocatable :: a1(:,:)
    class(*), allocatable :: s1(:,:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(a1(2,2),SOURCE=reshape((/(Child(i,i+1),i=11,14)/),(/2,2/)))
    allocate(s1(2,2),SOURCE=reshape((/1,-1,1,-2/),(/2,2/)))

    select type(s1)
        type is (integer)
            select type(name1=>eoshift(b1, s1, a1, 1))
                type is (Child)
                    print *, name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
