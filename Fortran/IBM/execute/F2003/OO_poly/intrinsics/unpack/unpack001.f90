!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR is non-poly. FIELD is scalar.
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

program unpack001
use m
    type(Base) :: b1(6)
    logical :: m1(3,4)

    b1 = (/(Base(i),i=1,6)/)
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/3,4/))

    print *, unpack(b1, m1, Base(-9))
    print *, shape(unpack(b1, m1, Base(-9)))
end
