!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/03/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : MOLD is a structure component, scalar or
!                              array. Non-poly.
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
        type(Base), pointer :: b1
        type(Base), allocatable :: b2(:,:)
    end type
end module

program structureComponent001
use m
    type(Child) :: c1(3)
    type(Child) :: c2(3)

    do i=1,3
        allocate(c1(i)%b1, SOURCE=Base(i))
        allocate(c1(i)%b2(i,i), SOURCE=Base(-i))
    end do

    do i=1,3
        c1(MOD(i,3)+1)%b1 => null(c1(i)%b1)
    end do

    do i=1,3
        if(associated(c1(i)%b1)) call zzrc(i)
    end do

    do i=1,3
        c2(i) = Child(i, null(c1(i)%b1), null(c1(i)%b2))
    end do

    do i=1,3
        if(associated(c2(i)%b1)) call zzrc(3+i)
        if(allocated(c2(i)%b2)) call zzrc(6+i)
    end do
end
