! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/reshape001.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
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
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program reshape001
use m
    type(Child(20,4,4)) :: c1(5)

    c1 = (/(Child(20,4,4)(i,i-1),i=101,105)/)

    associate(name1=>cshift(reshape(c1, (/3,2/), &
     (/Child(20,4,4)(-1,-2)/), (/1,2/)), (/1,2,3/), 2))
        if(.NOT. same_type_as(name1, Child(20,4,4)(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end