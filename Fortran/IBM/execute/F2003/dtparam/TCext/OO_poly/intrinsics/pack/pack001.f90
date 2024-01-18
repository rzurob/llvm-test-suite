! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/pack/pack001.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is non-poly. MASK is scalar or
!                              array. Vector is not present.
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

program pack001
use m
    type(Base(4)) :: b1(4,3)

    b1 = reshape((/(Base(4)(i),i=1,12)/), (/4,3/))

    print *, pack(b1, .TRUE.)
    print *, shape(pack(b1, .TRUE.))

    print *, pack(b1, same_type_as(b1, Child(4)(1,1)))
    print *, shape(pack(b1, same_type_as(b1, Child(4)(1,1))))

    print *, pack(b1, MOD(b1%i,2)==0)
    print *, shape(pack(b1, MOD(b1%i,2)==0))
end
