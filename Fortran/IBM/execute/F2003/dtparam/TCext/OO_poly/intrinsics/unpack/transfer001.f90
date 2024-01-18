! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/transfer001.f
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
!                              transfer. Non-poly.
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

program transfer001
use m
    type(Base(4)) :: v1(14)
    type(Base(4)) :: f1(3,4)

    v1 = (/(Base(4)(i),i=11,24)/)
    f1 = reshape((/(Base(4)(i),i=101,112)/),(/3,4/))

    associate(name1=>unpack(transfer(v1, (/Child(4)(1,1)/)), &
     reshape(MOD(f1%i,2)==1,(/6/),(/.FALSE.,.TRUE./)), &
     transfer(f1,(/Child(4)(1,1)/))))
        if(.NOT. same_type_as(name1, Child(4)(1,1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
