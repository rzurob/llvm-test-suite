! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/reshape001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/04/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is function return of reshape.
!                              Non-poly.
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

program reshape001
use m
    type(Child(4,20)) :: c1(5)

    c1 = (/(Child(4,20)(i,i-1),i=101,105)/)

    associate(name1=>eoshift(reshape(c1, (/3,2/), &
     (/Child(4,20)(-1,-2)/), (/1,2/)), (/1,2,3/), Child(4,20)(8,9), 2))
        if(.NOT. same_type_as(name1, Child(4,20)(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
