! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/structureComponent004.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is a structure component, which
!                              is a scalar. The object containing the
!                              component is an array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2)    ! (20,4,20)
        integer, len     :: n2
        type(Base(n2,4)) :: b1
    end type
end module

program structureComponent004
use m
    type(Child(20,4,20)) :: c1(4,5)

    c1 = reshape((/(Child(20,4,20)(i,Base(20,4)(-i)),i=101,120)/), (/4,5/))

    associate(name1=>eoshift(c1%b1, (/1,-2,2,-1/), c1(:,3)%Base, 2))
        if(.NOT. same_type_as(name1, Base(20,4)(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end