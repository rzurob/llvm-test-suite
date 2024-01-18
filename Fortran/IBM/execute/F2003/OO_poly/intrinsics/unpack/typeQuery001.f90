!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! DATE                       : 02/23/2005
! PRIMARY FUNCTIONS TESTED   : unpack
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program typeQuery001
use m
    class(AbstractParent), pointer :: b1(:)
    class(AbstractParent), allocatable :: b2(:,:)
    class(*), pointer :: b3(:) => null()
    logical :: m1(6)

    allocate(b1(4), SOURCE=(/(Child(i,-i),i=1,4)/))
    allocate(b2(2,3), SOURCE=reshape((/(Child(i,i),i=1,6)/), (/2,3/)))
    allocate(b3(5), SOURCE=(/(Child(-i,i),i=11,15)/))
    m1 = (/.FALSE.,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE./)

    if(.NOT. same_type_as(unpack(b1,reshape(m1,(/2,3/)),b2), &
     Child(1,1))) error stop 1_4

    if(.NOT. extends_type_of(unpack(b3,reshape(m1,(/2,3/), &
     ORDER=(/2,1/)),b2), Child(1,1))) error stop 2_4
end
