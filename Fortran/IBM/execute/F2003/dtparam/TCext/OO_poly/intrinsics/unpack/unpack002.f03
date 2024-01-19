! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/unpack002.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR is non-poly. FIELD is array.
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

program unpack002
use m
    type(Base(4)), pointer :: b1(:)
    logical :: m1(3,4)
    type(Base(4)), allocatable :: f1(:,:)

    allocate(b1(6), SOURCE=(/(Base(4)(i),i=1,6)/))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/3,4/))
    allocate(f1(3,4), SOURCE=reshape((/(Base(4)(i),i=101,112)/), &
     (/3,4/),ORDER=(/2,1/)))

    print *, unpack(b1, m1, f1)
    print *, shape(unpack(b1, m1, f1))
end
