!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/10/2005
! PRIMARY FUNCTIONS TESTED   : pack
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
    allocate(b3(5), SOURCE=(/(Child(-i,i),i=11,15)/))

    if(.NOT. same_type_as(pack(b1,.FALSE.), Child(1,1))) error stop 1_4

    if(.NOT. extends_type_of(pack(b2,MOD(b2%i,2)==0,b3), &
     Child(1,1))) error stop 2_4
end
