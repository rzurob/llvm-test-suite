!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DESCRIPTION                : ARRAY is the return value of a type
!                              bound procedure call.
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
        class(Base), allocatable :: createBase(:,:)
        allocate(createBase(3,4),SOURCE=reshape((/(Base(i),i=1,12)/), &
         (/3,4/)))
    end function

    function createChild(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChild(:,:)
        allocate(createChild(4,5),SOURCE=reshape((/(Child(i,-i), &
         i=1,20)/),(/4,5/)))
    end function
end module

program functionReturn002
use m
    class(Base), allocatable :: a
    class(*), pointer :: v1(:)
    logical :: m1(20)

    allocate(Base::a)
    allocate(v1(10), SOURCE=(/(Base(i),i=101,110)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE./)

    select type(name1=>pack(a%create(), reshape(m1,(/3,4/)), v1))
        type is (Base)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a, v1)
    allocate(Child::a)
    allocate(v1(12), SOURCE=(/(Child(i,-i),i=101,112)/))

    select type(name1=>pack(a%create(),reshape(m1,(/4,5/)), v1))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
