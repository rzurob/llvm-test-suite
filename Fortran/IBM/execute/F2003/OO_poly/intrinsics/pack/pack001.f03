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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program pack001
use m
    type(Base) :: b1(4,3)

    b1 = reshape((/(Base(i),i=1,12)/), (/4,3/))

    print *, pack(b1, .TRUE.)
    print *, shape(pack(b1, .TRUE.))

    print *, pack(b1, same_type_as(b1, Child(1,1)))
    print *, shape(pack(b1, same_type_as(b1, Child(1,1))))

    print *, pack(b1, MOD(b1%i,2)==0)
    print *, shape(pack(b1, MOD(b1%i,2)==0))
end
