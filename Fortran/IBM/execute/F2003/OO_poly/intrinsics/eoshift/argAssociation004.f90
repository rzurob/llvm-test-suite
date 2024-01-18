!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation004
use m
    type(Base), pointer :: b(:,:,:)
    type(Child), allocatable :: c(:,:)

    allocate(b(3,4,2), SOURCE=reshape((/(Base(i),i=1,24)/),(/3,4,2/)))
    allocate(c(2,3), SOURCE=reshape((/(Child(i,-i),i=3,8)/), &
     (/2,3/), (/Child(-1,-2)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base), pointer :: arg1(:,:,:)
        type(Child), allocatable :: arg2(:,:)

        print *, eoshift(arg1, reshape((/-1,2,-2,1,-1,1/),(/3,2/)), &
         reshape((/(Base(i),i=-6,-1)/),(/3,2/)), 2)
        print *, eoshift(arg2, (/1,-2/), Child(-88,-99), 2)
    end subroutine
end
