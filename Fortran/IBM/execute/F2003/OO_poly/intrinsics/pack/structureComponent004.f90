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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        type(Base) :: b1
    end type
end module

program structureComponent004
use m
    type(Child) :: c1(4,5)

    c1 = reshape((/(Child(i,Base(-i)),i=101,120)/), (/4,5/))

    associate(name1=>pack(c1%b1, MOD(c1%i,2)==1))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
