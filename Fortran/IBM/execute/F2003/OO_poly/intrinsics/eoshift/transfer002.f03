!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : Function return of eoshift is the SOURCE
!                              of transfer. Non-poly.
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

program transfer002
use m
    type(Base) :: c1(5,4)

    c1 = reshape((/(Base(i),i=1,20)/), (/5,4/))

    associate(name1=>transfer(eoshift(c1,(/1,-2,3,-1,2/), &
     (/(Base(i),i=31,35)/),2), (/Child(1,1)/)))
        if(.NOT. same_type_as(name1, Child(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
