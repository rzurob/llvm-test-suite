! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/pack/argumentKeyword002.f
! opt variations: -ql -qreuse=none

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 9
    end type
end module

program argumentKeyword002
use m
    class(*), pointer :: b1(:,:,:)
    class(*), pointer :: v1(:)
    allocate(b1(3,2,2), SOURCE=reshape((/(Child(4)(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(v1(8), SOURCE=(/(Child(4)(i,-i),i=101,108)/))

    select type(b1)
        class is (Base(4))
            select type(name1=>pack(VECTOR=v1,MASK=(MOD(b1%i,2)==0), &
             ARRAY=b1))
                type is (Child(4))
                    print *, name1
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select
end
