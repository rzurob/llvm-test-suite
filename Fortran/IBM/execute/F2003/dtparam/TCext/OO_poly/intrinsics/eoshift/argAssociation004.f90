! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/argAssociation004.f
!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
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

        print *, eoshift(arg1, reshape((/-1,2,-2,1,-1,1/),(/3,2/)), &
         reshape((/(Base(4)(i),i=-6,-1)/),(/3,2/)), 2)
        print *, eoshift(arg2, (/1,-2/), Child(4,4)(-88,-99), 2)
    end subroutine
end
