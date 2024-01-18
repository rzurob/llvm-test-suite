! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/unpack/reshape002.f
! opt variations: -qnok -qnol -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/24/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
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
    type(Child(4,20)) :: v1(12)

    c1 = reshape((/(Child(4,20)(i,i-1),i=1,20)/), (/5,4/))
    v1 = (/(Child(4,20)(i,-i),i=101,112)/)

    print *, reshape(unpack(v1,MOD(c1%j,2)==0,c1), &
     (/3,3,2/), (/Child(4,20)(-8,-9)/), (/1,2,3/))
end
