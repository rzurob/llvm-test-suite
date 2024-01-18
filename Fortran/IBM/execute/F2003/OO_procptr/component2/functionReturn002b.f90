!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using declaration type
!                              specification. Poly, dummy arguments are
!                              pointer, and return is pointer.
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
        integer i
        !procedure(type(Base)), pointer, nopass :: pp1
         procedure(func2), pointer, nopass :: pp1
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function func1(b)
        class(AbstractParent), pointer, intent(in) :: b
        !procedure(type(Base)), pointer :: func1
         procedure(func2), pointer :: func1

        select type (b)
            type is (Base)
                func1 => func2
            type is (Child)
                func1 => func3
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(AbstractParent), pointer, intent(in) :: b
        type(Base), pointer :: func2
        select type (b)
            type is (Base)
                allocate(func2, SOURCE=Base(b%i, null()))
            type is (Child)
                allocate(func2, SOURCE=Base(b%i+b%j, null()))
            class default
                error stop 2_4
        end select
    end function

    function func3(b)
        class(AbstractParent), pointer, intent(in) :: b
        type(Base), pointer :: func3
        select type (b)
            type is (Base)
                allocate(func3, SOURCE=Base(-b%i, null()))
            type is (Child)
                allocate(func3, SOURCE=Base(b%i-b%j, null()))
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn002b
use m
    class(AbstractParent), pointer :: b1
    class(Base), allocatable :: b2

    allocate(Child::b2)
    allocate(b1, SOURCE=Base(5, null()))
    b2%pp1 => func1(b1)
    associate(name1=>b2%pp1(b1))
        print *, "func2", name1
    end associate

    deallocate(b1)
    allocate(b1, SOURCE=Child(12, null(), 21))
    b2%pp1 => func1(b1)
    associate(name2=>b2%pp1(b1))
        print *, "func3", name2
    end associate
end
