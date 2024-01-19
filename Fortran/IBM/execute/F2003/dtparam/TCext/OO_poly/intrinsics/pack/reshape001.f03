! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/pack/reshape001.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is function return of reshape.
!                              Non-poly.
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

program reshape001
use m
    type(Child(4)) :: c1(5)
    type(Child(4)) :: v1(7)

    c1 = (/(Child(4)(i,i-1),i=101,105)/)
    v1 = (/(Child(4)(i,-i),i=11,17)/)

    associate(name1=>pack(reshape(c1,(/3,2/),(/Child(4)(-1,-2)/), &
     (/2,1/)),reshape(MOD(c1%i,2)==1,(/3,2/),(/.TRUE./)),v1))
        if(.NOT. same_type_as(name1, Child(4)(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
