! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/component2/functionReturn002b.f
! opt variations: -qnok -ql -qreuse=none

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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
        !procedure(type(Base)), pointer, nopass :: pp1
         procedure(func2), pointer, nopass :: pp1
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    function func1(b)
        class(AbstractParent(4)), pointer, intent(in) :: b
        !procedure(type(Base)), pointer :: func1
         procedure(func2), pointer :: func1

        select type (b)
            type is (Base(4))
                func1 => func2
            type is (Child(4))
                func1 => func3
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(AbstractParent(4)), pointer, intent(in) :: b
        type(Base(4)), pointer :: func2
        select type (b)
            type is (Base(4))
                allocate(func2, SOURCE=Base(4)(b%i, null()))
            type is (Child(4))
                allocate(func2, SOURCE=Base(4)(b%i+b%j, null()))
            class default
                error stop 2_4
        end select
    end function

    function func3(b)
        class(AbstractParent(4)), pointer, intent(in) :: b
        type(Base(4)), pointer :: func3
        select type (b)
            type is (Base(4))
                allocate(func3, SOURCE=Base(4)(-b%i, null()))
            type is (Child(4))
                allocate(func3, SOURCE=Base(4)(b%i-b%j, null()))
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn002b
use m
    class(AbstractParent(4)), pointer :: b1
    class(Base(4)), allocatable :: b2

    allocate(Child(4)::b2)
    allocate(b1, SOURCE=Base(4)(5, null()))
    b2%pp1 => func1(b1)
    associate(name1=>b2%pp1(b1))
        print *, "func2", name1
    end associate

    deallocate(b1)
    allocate(b1, SOURCE=Child(4)(12, null(), 21))
    b2%pp1 => func1(b1)
    associate(name2=>b2%pp1(b1))
        print *, "func3", name2
    end associate
end
