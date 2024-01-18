!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Function return of unpack is the SOURCE
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
    type(Base) :: v1(14)
    type(Base) :: f1(5,4)

    v1 = (/(Base(i), i=101,114)/)
    f1 = reshape((/(Base(i),i=1,20)/), (/5,4/))

    associate(name1=>transfer(unpack(v1, MOD(f1%i,2)==0, f1), &
     (/Child(1,1)/)))
        if(.NOT. same_type_as(name1, Child(1,1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
