!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is the return value of a type bound
!                              procedure call.
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

        contains
        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child
        integer j

        contains
        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base), intent(in) :: a
        class(*), pointer :: createBase(:,:)
        allocate(createBase(3,4),SOURCE=reshape((/(Base(i),i=1,12)/), &
         (/3,4/)))
    end function

    function createChild(a)
        class(Child), intent(in) :: a
        class(*), pointer :: createChild(:,:)
        allocate(createChild(4,5),SOURCE=reshape((/(Child(i,-i), &
         i=1,20)/),(/4,5/)))
    end function
end module

program functionReturn003
use m
    class(Base), allocatable :: a

    allocate(Base::a)

    if(associated(null(a%create()))) error stop 1_4
    if(.NOT. same_type_as(a%create(), Base(1))) error stop 2_4
    if(same_type_as(null(a%create()), Base(1))) error stop 3_4
    if(.NOT. extends_type_of(Base(1), null(a%create()))) error stop 4_4

    deallocate(a)
    allocate(Child::a)

    if(associated(null(a%create()))) error stop 5_4
    if(.NOT. same_type_as(a%create(), Child(1,1))) error stop 6_4
    if(same_type_as(null(a%create()), Base(1))) error stop 7_4
    if(.NOT. extends_type_of(Base(1), null(a%create()))) error stop 8_4
end