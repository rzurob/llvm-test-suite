!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
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

    allocate(Base::a)

    select type(name1=>eoshift(a%create(), -1, &
     (/Base(-3),Base(-4),Base(-5)/), 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child::a)

    select type(name1=>eoshift(a%create(),(/1,-2,2,-1,1/), &
     (/(Child(i,i+1),i=-5,-1)/)))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
