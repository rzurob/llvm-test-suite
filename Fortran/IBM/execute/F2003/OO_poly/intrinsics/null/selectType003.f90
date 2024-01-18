!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Return of null is the selector of a
!                              select type construct. MOLD is unlimited
!                              poly.
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

program selectType003
use m
    class(*), allocatable :: u1(:,:)
    allocate(u1(4,5),SOURCE=reshape((/(Child(i,-i),i=1,20)/),(/4,5/)))

    select type(name1=>null(u1))
        type is (Child)
            error stop 1_4
        type is (Base)
            error stop 2_4
        class is (AbstractParent)
            error stop 3_4
        class default
            ! should come here, but do nothing
    end select
end
