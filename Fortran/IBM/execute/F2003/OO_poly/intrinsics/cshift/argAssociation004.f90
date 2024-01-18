!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
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

        print *, cshift(arg1, reshape((/-1,2,-3,4,-5,6/),(/3,2/)),2)
        print *, shape(cshift(arg1, reshape((/-1,2,-3,4,-5,6/), &
         (/3,2/)),2))
        print *, cshift(arg2, (/1,-2/), 2)
        print *, shape(cshift(arg2, (/1,-2/), 2))
    end subroutine
end
