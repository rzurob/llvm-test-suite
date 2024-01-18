!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Function return of pack is the SOURCE
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

    print *, reshape(pack(c1,MOD(c1%j,2)==0,v1), &
     (/3,3,2/), (/Child(-8,-9)/), (/1,2,3/))

    associate(name1=>reshape(pack(c1,MOD(c1%j,2)==0,v1), &
     (/3,3,2/), (/Child(-8,-9)/), (/1,2,3/)))
        if(.NOT. same_type_as(name1, Child(1,1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
