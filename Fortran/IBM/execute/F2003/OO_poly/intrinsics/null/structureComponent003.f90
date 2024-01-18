!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/03/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : MOLD is a structure component, scalar or
!                              array. Unlimited poly.
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

    type Container
        class(*), pointer :: b1
        class(*), allocatable :: b2(:,:)
    end type
end module

program structureComponent002
use m
    type(Container) :: c1(3)
    type(Container) :: c2(3)

    do i=1,3
        allocate(c1(i)%b1, SOURCE=Base(i))
        allocate(c1(i)%b2(i,i), SOURCE=Child(-i,-i))
    end do

    do i=1,3
        if(.NOT. associated(c1(i)%b1)) error stop 1_4
        if(.NOT. allocated(c1(i)%b2)) error stop 2_4
        if(.NOT. same_type_as(c1(i)%b1, Base(1))) error stop 3_4
        if(.NOT. same_type_as(c1(i)%b2, Child(1,1))) error stop 4_4
    end do

    do i=1,3
        c1(MOD(i,3)+1)%b1 => null(c1(i)%b1)
    end do

    do i=1,3
        if(associated(c1(i)%b1)) error stop 5_4
        if(same_type_as(c1(i)%b1, Base(1))) error stop 6_4
        if(.NOT. extends_type_of(Base(1), c1(i)%b1)) error stop 7_4
    end do

    do i=1,3
        c2(i) = Container(null(c1(i)%b1), null(c1(i)%b2))
    end do

    do i=1,3
        if(associated(c2(i)%b1)) error stop 8_4
        if(allocated(c2(i)%b2)) error stop 9_4
        if(same_type_as(c2(i)%b1, Base(1))) error stop 10_4
        if(same_type_as(c2(i)%b2, Child(1,1))) error stop 11_4
        if(.NOT. extends_type_of(Base(1), c2(i)%b1)) error stop 12_4
        if(.NOT. extends_type_of(Base(1), c2(i)%b2)) error stop 13_4
    end do
end
