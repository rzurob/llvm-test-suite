!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : Function return of cshift is the SOURCE
!                              of reshape. Non-poly.
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
        integer j
    end type
end module

program reshape002
use m
    type(Child) :: c1(5,4)

    c1 = reshape((/(Child(i,-i),i=1,20)/), (/5,4/))

    associate(name1=>reshape(cshift(c1,(/1,-2,3,-4,5/),2), (/3,3,3/), &
     (/Child(-8,-9)/), (/1,2,3/)))
        if(.NOT. same_type_as(name1, Child(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end