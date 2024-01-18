!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/18/2005
! PRIMARY FUNCTIONS TESTED   : pack
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
    type(Child) :: c1(3,4)
    type(Base) :: v1(14)

    c1 = reshape((/(Child(i,i-1),i=101,112)/),(/3,4/))
    v1 = (/(Base(i),i=11,24)/)

    associate(name1=>pack(transfer(c1, (/Base(1)/)), &
     reshape(MOD(c1%i,2)==1,(/24/),(/.FALSE.,.TRUE./)), v1))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
