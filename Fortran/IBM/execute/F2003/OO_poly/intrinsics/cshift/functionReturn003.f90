!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/cshift
! DATE                       : 02/02/2005
! PRIMARY FUNCTIONS TESTED   : cshift
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
        class(Base), allocatable :: createBase(:)
        allocate(createBase(10), SOURCE=(/ (Base(i), i=1,10) /))
    end function

    function createChild(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChild(:)
        allocate(createChild(20), SOURCE=(/ (Child(i,-i), i=1,20) /))
    end function
end module

program functionReturn003
use m
    class(Base), allocatable :: a

    allocate(Base::a)

    select type(name1=>cshift(a%create(), 5, 1))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child::a)

    select type(name1=>cshift(a%create(), -12))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
