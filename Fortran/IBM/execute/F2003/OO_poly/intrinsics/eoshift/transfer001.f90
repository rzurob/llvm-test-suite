!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is function return of transfer.
!                              Non-poly.
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

program transfer001
use m
    type(Child) :: c1(5)

    c1 = (/(Child(i,i-1),i=101,105)/)

    print *, eoshift(transfer(c1, (/Base(1)/)), 5, Base(8), 1)
    print *, shape(eoshift(transfer(c1, (/Base(1)/)), 5, Base(8), 1))
    associate(name1=>eoshift(transfer(c1, (/Base(1)/)), 5, Base(8), 1))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
