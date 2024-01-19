!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 01/31/2005
! PRIMARY FUNCTIONS TESTED   : cshift
! DESCRIPTION                : Use EXTENDS_TYPE_OF and SAME_TYPE_AS to
!                              check the return value.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program typeQuery001
use m
    class(Base), pointer :: b1(:,:,:)
    class(Base), allocatable :: b2(:,:)
    class(*), pointer :: b3(:) => null()

    allocate(b1(3,2,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/3,2,2/)))
    allocate(b2(2,3), SOURCE=reshape((/(Child(i,i),i=1,6)/), (/2,3/)))
    allocate(Base::b3(3))

    if(.NOT. same_type_as(cshift(b1,3,2), Child(1,1))) error stop 1_4

    if(.NOT. extends_type_of(cshift(b2,(/1,2,3/),1), Base(1))) &
     error stop 2_4

    if(.NOT. same_type_as(cshift(b3,-5), Base(1))) error stop 3_4
end
