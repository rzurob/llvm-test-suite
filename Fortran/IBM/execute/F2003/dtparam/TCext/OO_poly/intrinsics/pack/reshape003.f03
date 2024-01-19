! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/pack/reshape003.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is function return of reshape.
!                              Poly and unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program reshape003
use m
    class(AbstractParent(4)), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)
    class(*), pointer :: v1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(4)(i,i-1),i=101,108)/), &
     (/2,2,2/)))

    select type(name1=>pack(reshape(c1,(/3,4/),(/Child(4)(-1,-2), &
     Child(4)(-3,-4)/),(/2,1/)),reshape((MOD((/(i,i=1,12)/),2)==1), &
     (/3,4/))))
        type is (Child(4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(12), SOURCE=(/(Child(4)(i,-i),i=1,12)/))
    allocate(v1(15), SOURCE=(/(Child(4)(i,-i),i=101,115)/))

    select type(name1=>pack(reshape(b1, (/3,2,2/)), .TRUE., v1))
        type is (Child(4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
