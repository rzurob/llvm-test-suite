! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/eoshift/transfer002.f
! opt variations: -qnok -qnol -qreuse=none

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

program transfer002
use m
    type(Base(4,20)) :: c1(5,4)

    c1 = reshape((/(Base(4,20)(i),i=1,20)/), (/5,4/))

    associate(name1=>transfer(eoshift(c1,(/1,-2,3,-1,2/), &
     (/(Base(4,20)(i),i=31,35)/),2), (/Child(4,20)(1,1)/)))
        if(.NOT. same_type_as(name1, Child(4,20)(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
