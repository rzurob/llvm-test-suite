! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/OO_poly/intrinsics/pack/structureComponent004.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is a structure component, which
!                              is a scalar. The object containing the
!                              component is an array. Non-poly.
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

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(k3,n3)    ! (4,20,20,4,4,20)
        integer, kind           :: k3
        integer, len            :: n3
        type(Base(k3,n3,n3,k3)) :: b1
    end type
end module

program structureComponent004
use m
    type(Child(4,20,20,4,4,20)) :: c1(4,5)

    c1 = reshape((/(Child(4,20,20,4,4,20)(i,Base(4,20,20,4)(-i)),i=101,120)/), (/4,5/))

    associate(name1=>pack(c1%b1, MOD(c1%i,2)==1))
        if(.NOT. same_type_as(name1, Base(4,20,20,4)(1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
