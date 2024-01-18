! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/structureComponent001.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a structure component, which
!                              is non-poly array. The object containing
!                              the component is a scalar.
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

    type Base1(k2)    ! (4)
        integer, kind :: k2
        integer(k2)      m
        integer(k2)      n
    end type

    type, extends(Base) :: Child    ! (4,20)
        type(Base(k1,n1)) :: b1(20)
        type(Base1(k1)) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child(4,20)) :: c1

    c1%b1 = (/ (Base(4,20)(i), i=1,20) /)
    c1%b2 = reshape((/(Base1(4)(i,-i),i=1,20)/), (/5,5/), &
     (/Base1(4)(88,-88),Base1(4)(99,-99)/), (/2,1/))

    associate(name1=>cshift(c1%b1, 12))
        if(.NOT. same_type_as(name1, Base(4,20)(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    associate(name1=>cshift(c1%b2, (/-1,2,-3,4,-5/), 2))
        if(.NOT. same_type_as(name1, Base1(4)(1,1))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
