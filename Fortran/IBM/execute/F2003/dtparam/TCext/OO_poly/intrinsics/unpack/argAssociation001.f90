! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/unpack/argAssociation001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DESCRIPTION                : VECTOR or FIELD is a dummy argument. Dummy
!                              argument is non-pointer, non-allocatable,
!                              non-poly, and is array.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program argAssociation001
use m
    type(Base(20,4)) :: b(10)
    class(Base(:,4)), pointer :: c(:,:)

    b = (/ (Base(20,4)(i),i=1,10) /)

    allocate(c(3,2), SOURCE=reshape((/(Child(20,4)(j=i,i=-i),i=1,6)/),(/3,2/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base(*,4)) :: arg1(10)
        type(Base(*,4)) :: arg2(:,:)

        print *, unpack(arg1, reshape((/.TRUE.,.FALSE.,.FALSE.,.TRUE., &
         .TRUE.,.FALSE./),(/3,2/)), arg2)
    end subroutine
end
