!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Use EXTENDS_TYPE_OF and SAME_TYPE_AS to
!                              check the return value. Poly and unlimited
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
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program typeQuery002
use m
    class(AbstractParent), allocatable :: b1
    class(*), allocatable :: b2
    class(*), pointer :: b3(:,:)

    if(same_type_as(b1, b2)) error stop 1_4
    if(same_type_as(b2, b3)) error stop 2_4
    if(same_type_as(null(b1), null(b2))) error stop 3_4
    if(same_type_as(null(b2), null(b3))) error stop 4_4
    if(same_type_as(null(b1), b2)) error stop 5_4
    if(same_type_as(null(b2), b3)) error stop 6_4
    if(same_type_as(null(b3), b1)) error stop 7_4

    if(.NOT. extends_type_of(b1, b2)) error stop 8_4
    if(.NOT. extends_type_of(b2, b3)) error stop 9_4
    if(.NOT. extends_type_of(null(b1), null(b2))) error stop 10_4
    if(.NOT. extends_type_of(null(b2), null(b3))) error stop 11_4
    if(.NOT. extends_type_of(null(b1), b2)) error stop 12_4
    if(.NOT. extends_type_of(null(b2), b3)) error stop 13_4
    if(extends_type_of(null(b3), b1)) error stop 14_4

    allocate(Child::b1)
    allocate(b2, SOURCE=Child(1,2))
    allocate(b3(3,8), SOURCE=reshape((/(Child(i,-i),i=1,24)/),(/3,8/)))

    if(.NOT. same_type_as(b1, b2)) error stop 15_4
    if(.NOT. same_type_as(b2, b3)) error stop 16_4
    if(same_type_as(null(b1), null(b2))) error stop 17_4
    if(same_type_as(null(b2), null(b3))) error stop 18_4
    if(same_type_as(null(b1), b2)) error stop 19_4
    if(same_type_as(null(b2), b3)) error stop 20_4
    if(same_type_as(null(b3), b1)) error stop 21_4

    if(.NOT. extends_type_of(b1, b2)) error stop 22_4
    if(.NOT. extends_type_of(b2, b3)) error stop 23_4
    if(.NOT. extends_type_of(null(b1), null(b2))) error stop 24_4
    if(.NOT. extends_type_of(null(b2), null(b3))) error stop 25_4
    if(extends_type_of(null(b1), b2)) error stop 26_4
    if(extends_type_of(null(b2), b3)) error stop 27_4
    if(extends_type_of(null(b3), b1)) error stop 28_4
end
