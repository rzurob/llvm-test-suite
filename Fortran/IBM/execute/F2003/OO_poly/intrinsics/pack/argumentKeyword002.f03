!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Actual arguments are speficied using
!                              argument keywords. Unlimited poly.
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

program argumentKeyword002
use m
    class(*), pointer :: b1(:,:,:)
    class(*), pointer :: v1(:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(v1(8), SOURCE=(/(Child(i,-i),i=101,108)/))

    select type(b1)
        class is (Base)
            select type(name1=>pack(VECTOR=v1,MASK=(MOD(b1%i,2)==0), &
             ARRAY=b1))
                type is (Child)
                    print *, name1
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end