! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/pack/reshape002.f
! opt variations: -qnok -qnol -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : Function return of pack is the SOURCE
!                              of reshape. Non-poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program reshape002
use m
    type(Child(4,20)) :: c1(5,4)
    type(Child(4,20)) :: v1(12)

    c1 = reshape((/(Child(4,20)(i,i-1),i=1,20)/), (/5,4/))
    v1 = (/(Child(4,20)(i,-i),i=101,112)/)

    print *, reshape(pack(c1,MOD(c1%j,2)==0,v1), &
     (/3,3,2/), (/Child(4,20)(-8,-9)/), (/1,2,3/))

    associate(name1=>reshape(pack(c1,MOD(c1%j,2)==0,v1), &
     (/3,3,2/), (/Child(4,20)(-8,-9)/), (/1,2,3/)))
        if(.NOT. same_type_as(name1, Child(4,20)(1,1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
