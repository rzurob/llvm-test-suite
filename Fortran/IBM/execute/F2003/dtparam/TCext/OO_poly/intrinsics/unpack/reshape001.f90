! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/reshape001.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is function return of
!                              reshape. Non-poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program reshape001
use m
    type(Child(4)) :: v1(7)
    type(Child(4)) :: f1(15)

    v1 = (/(Child(4)(i,-i),i=11,17)/)
    f1 = (/(Child(4)(i,i-1),i=101,115)/)

    associate(name1=>unpack(v1, reshape(MOD(f1%i,2)==1,(/3,4/), &
     (/.TRUE./)), reshape(f1,(/3,4/),(/Child(4)(-1,-2)/), (/2,1/))))
        if(.NOT. same_type_as(name1, Child(4)(1,1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
