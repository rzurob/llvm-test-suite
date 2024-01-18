! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/reshape002.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program reshape002
use m
    type(Child(4,20)) :: c1(5,4)

    c1 = reshape((/(Child(4,20)(i,-i),i=1,20)/), (/5,4/))

    associate(name1=>reshape(cshift(c1,(/1,-2,3,-4,5/),2), (/3,3,3/), &
     (/Child(4,20)(-8,-9)/), (/1,2,3/)))
        if(.NOT. same_type_as(name1, Child(4,20)(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
