!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface. Associate the
!                              procedure pointer to a function, with
!                              array dummy arguments. Unlimited poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    implicit type(Base) (p)

    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
        procedure(func2), pointer, nopass :: pp1 => null()
    end type

    contains

    function func1(b)
        class(*), pointer :: b(:,:)
        procedure(func2), pointer :: func1

        select type (b)
            type is (Base)
                func1 => func2
            type is (Child)
                func1 => func3
            class default
                error stop 5_4
        end select
    end function

    function func2(b)
        class(*), pointer :: b(:,:)
        type(Base), allocatable :: func2

        select type (b)
            type is (Base)
                allocate(func2, SOURCE=Base(sum(b%i)))
            class default
                error stop 6_4
        end select
    end function

    function func3(b)
        class(*), pointer :: b(:,:)
        type(Base), allocatable :: func3

        select type (b)
            type is (Child)
                allocate(func3, SOURCE=Base(sum(b%i)*sum(b%j)))
            class default
                error stop 7_4
        end select
    end function
end module

program functionReturn003b
use m
    type(Child), pointer :: c1
    class(*), pointer :: b1(:,:)

    allocate(Child::c1)
    if(associated(c1%pp1)) error stop 1_4

    allocate(b1(2,3), SOURCE=reshape((/(Base(i),i=1,6)/),(/2,3/)))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 2_4
    print *, "func2", c1%pp1(b1)

    c1%pp1 => null()
    if(associated(c1%pp1)) error stop 3_4

    deallocate(b1)
    allocate(b1(5,3), SOURCE=reshape((/(Child(i,-i,null()),i=1,15)/), &
     (/5,3/)))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 4_4
    print *, "func3", c1%pp1(b1)
    deallocate(b1)
end
