! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/cshift/argAssociation004.f
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program argAssociation004
use m
    type(Base(4)), pointer :: b(:,:,:)
    type(Child(4,4)), allocatable :: c(:,:)

    allocate(b(3,4,2), SOURCE=reshape((/(Base(4)(i),i=1,24)/),(/3,4,2/)))
    allocate(c(2,3), SOURCE=reshape((/(Child(4,4)(i,-i),i=3,8)/), &
     (/2,3/), (/Child(4,4)(-1,-2)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base(4)), pointer :: arg1(:,:,:)
        type(Child(4,4)), allocatable :: arg2(:,:)

        print *, cshift(arg1, reshape((/-1,2,-3,4,-5,6/),(/3,2/)),2)
        print *, shape(cshift(arg1, reshape((/-1,2,-3,4,-5,6/), &
         (/3,2/)),2))
        print *, cshift(arg2, (/1,-2/), 2)
        print *, shape(cshift(arg2, (/1,-2/), 2))
    end subroutine
end