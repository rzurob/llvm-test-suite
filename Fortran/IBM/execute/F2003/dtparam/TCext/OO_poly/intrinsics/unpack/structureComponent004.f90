! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/unpack/structureComponent004.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR or FIELD is a structure component,
!                              which is a scalar. The object containing
!                              the component is an array. Non-poly.
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
        type(Base(k1)) :: b1
        type(Base(k1)) :: v1
    end type
end module

program structureComponent004
use m
    type(Child(4)) :: c1(4,5)
    type(Child(4)) :: c2(12)

    c1 = reshape((/(Child(4)(i,Base(4)(-i),Base(4)(i)),i=101,120)/), (/4,5/))
    c2 = (/(Child(4)(i,Base(4)(-i),Base(4)(i)),i=1,12)/)

    associate(name1=>unpack(c2%v1, MOD(c1%i,2)==1, c1%b1))
        if(.NOT. same_type_as(name1, Base(4)(1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
