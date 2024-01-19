! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/pack/associate001.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY, MASK, or VECTOR is an associate
!                              name.
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

program associate001
use m
    class(AbstractParent(4)), pointer :: ap1(:,:) => null()
    class(*), allocatable :: v1(:)
    logical :: m1(5,5)

    allocate(ap1(4,5),SOURCE=reshape((/(Child(4)(i,-i),i=1,20)/),(/4,5/)))
    allocate(v1(12),SOURCE=(/(Child(4)(i,i+1),i=11,22)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE., &
     .FALSE.,.TRUE./), (/5,5/), (/.FALSE.,.TRUE./))

    associate(name1=>ap1,name2=>m1(2:,:),name3=>v1(:))
        select type(name4=>pack(name1,name2,name3))
            type is (Child(4))
                print *, name4
                print *, shape(name4)
            class default
                error stop 1_4
        end select
    end associate
end
