! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/unpack/argAssociation004.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR or FIELD is a dummy argument. Dummy
!                              argument is a pointer or allocatable,
!                              non-poly, and is array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation004
use m
    type(Child(4)), pointer :: b(:,:,:)
    type(Child(4)), allocatable :: c(:)

    allocate(b(3,4,2), SOURCE=reshape((/(Child(4)(i,i-1),i=1,24)/), &
     (/3,4,2/), ORDER=(/2,1,3/)))
    allocate(c(14), SOURCE=(/(Child(4)(i,-i),i=1,14)/))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Child(4)), pointer :: arg1(:,:,:)
        type(Child(4)), allocatable :: arg2(:)

        print *, unpack(arg2, MOD(arg1%i,2)==1, arg1)
    end subroutine
end