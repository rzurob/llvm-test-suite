!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is the return value of a
!                              type bound procedure call.
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
        procedure, pass :: createOne => createBaseOne
        procedure, pass :: createTwo => createBaseTwo
    end type

    type, extends(Base) :: Child
        integer j

        contains
        procedure, pass :: createOne => createChildOne
        procedure, pass :: createTwo => createChildTwo
    end type

    contains

    function createBaseOne(a)
        class(Base), intent(in) :: a
        class(Base), allocatable :: createBaseOne(:)
        allocate(createBaseOne(10),SOURCE=(/(Base(-i),i=1,10)/))
    end function

    function createBaseTwo(a)
        class(Base), intent(in) :: a
        class(Base), allocatable :: createBaseTwo(:,:)
        allocate(createBaseTwo(3,4), &
         SOURCE=reshape((/(Base(i),i=1,12)/), (/3,4/)))
    end function

    function createChildOne(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChildOne(:)
        allocate(createChildOne(9),SOURCE=(/(Child(-i,i), i=11,19)/))
    end function

    function createChildTwo(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChildTwo(:,:)
        allocate(createChildTwo(4,5),SOURCE=reshape((/(Child(i,-i), &
         i=1,20)/),(/4,5/)))
    end function
end module

program functionReturn002
use m
    class(Base), allocatable :: a
    logical :: m1(20)

    allocate(Base::a)
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
     .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE./)

    select type(name1=>unpack(a%createOne(), reshape(m1,(/3,4/)), &
     a%createTwo()))
        type is (Base)
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child::a)

    select type(name1=>unpack(a%createOne(), reshape(m1,(/4,5/)), &
     a%createTwo()))
        type is (Child)
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
