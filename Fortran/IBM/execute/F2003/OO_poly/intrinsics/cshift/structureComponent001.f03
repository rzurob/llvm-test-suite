!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is a structure component, which
!                              is non-poly array. The object containing
!                              the component is a scalar.
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

    type Base1
        integer m
        integer n
    end type

    type, extends(Base) :: Child
        type(Base) :: b1(20)
        type(Base1) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child) :: c1

    c1%b1 = (/ (Base(i), i=1,20) /)
    c1%b2 = reshape((/(Base1(i,-i),i=1,20)/), (/5,5/), &
     (/Base1(88,-88),Base1(99,-99)/), (/2,1/))

    associate(name1=>cshift(c1%b1, 12))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    associate(name1=>cshift(c1%b2, (/-1,2,-3,4,-5/), 2))
        if(.NOT. same_type_as(name1, Base1(1,1))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
