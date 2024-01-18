! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/pack/pack002.f
! opt variations: -qnok -qnol -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is non-poly. MASK is scalar or
!                              array. Vector is present.
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

program pack002
use m
    type(Base(4,20)) :: b1(4,3)
    type(Base(4,20)) :: v1(13)

    b1 = reshape((/(Base(4,20)(i),i=1,12)/), (/4,3/))
    v1 = (/(Base(4,20)(i),i=-1,-13,-1)/)

    print *, pack(b1, .TRUE., v1)
    print *, shape(pack(b1, .TRUE., v1))

    print *, pack(b1, same_type_as(b1, Child(4,20)(1,1)), v1)
    print *, shape(pack(b1, same_type_as(b1, Child(4,20)(1,1)), v1))

    print *, pack(b1, MOD(b1%i,2)==0, v1)
    print *, shape(pack(b1, MOD(b1%i,2)==0, v1))
end
