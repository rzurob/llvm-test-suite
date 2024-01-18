!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : Function return of unpack is the SOURCE
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
    type(Child) :: v1(12)

    c1 = reshape((/(Child(i,i-1),i=1,20)/), (/5,4/))
    v1 = (/(Child(i,-i),i=101,112)/)

    print *, reshape(unpack(v1,MOD(c1%j,2)==0,c1), &
     (/3,3,2/), (/Child(-8,-9)/), (/1,2,3/))
end
