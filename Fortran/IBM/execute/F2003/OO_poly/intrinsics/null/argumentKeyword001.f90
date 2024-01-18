!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : Actual arguments are speficied using
!                              argument keywords. Poly and unlimited
!                              poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program argumentKeyword001
use m
    class(Base), pointer :: b1(:,:,:)
    class(*), pointer :: b2(:,:,:)

    allocate(b1(3,2,2), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
     (/3,2,2/)))

    if(.NOT. same_type_as(null(MOLD=b1), Base(1))) error stop 1_4
    if(same_type_as(null(MOLD=b1), b1)) error stop 2_4
    if(.NOT. extends_type_of(b1, null(MOLD=b1))) error stop 3_4
    if(.NOT. associated(b1)) error stop 4_4

    if(same_type_as(null(MOLD=b2), b2)) error stop 5_4
    if(.NOT. extends_type_of(b2, null(MOLD=b2))) error stop 6_4

    b2 => null(MOLD=b1)
    if(same_type_as(b2, Child(1,1))) error stop 7_4
    if(same_type_as(b2, Base(1))) error stop 8_4
    if(same_type_as(null(MOLD=b2), b2)) error stop 9_4
    if(.NOT. extends_type_of(b2, null(MOLD=b2))) error stop 10_4
    if(associated(b2)) error stop 11_4
end
