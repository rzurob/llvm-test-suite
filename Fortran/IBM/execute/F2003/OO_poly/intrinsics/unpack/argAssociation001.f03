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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation001
use m
    type(Base) :: b(10)
    class(Base), pointer :: c(:,:)

    b = (/ (Base(i),i=1,10) /)

    allocate(c(3,2), SOURCE=reshape((/(Child(j=i,i=-i),i=1,6)/),(/3,2/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base) :: arg1(10)
        type(Base), CONTIGUOUS :: arg2(:,:)

        print *, unpack(arg1, reshape((/.TRUE.,.FALSE.,.FALSE.,.TRUE., &
         .TRUE.,.FALSE./),(/3,2/)), arg2)
    end subroutine
end