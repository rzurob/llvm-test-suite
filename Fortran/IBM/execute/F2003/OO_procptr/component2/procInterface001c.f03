!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The procedure pointer
!                              is a dummy argument. The associated
!                              function is a module function. Poly and
!                              unlimited poly. Intrinsic or derived
!                              type, scalar.
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
    end type

    type, extends(Base) :: Child
        integer j
        procedure(func1), pointer, nopass :: pp1
        procedure(func2), pointer, nopass :: pp2
    end type

    type(Child) :: c1

    contains

    type(Base) function func1(b)
        class(AbstractParent), allocatable, intent(in) :: b
        select type (b)
            type is (Base)
                func1 = Base(b%i*2)
            type is (Child)
                func1 = Base(b%i+b%j)
            class default
                error stop 1_4
        end select
    end function

    type(Base) function func2(b, p)
        class(AbstractParent), allocatable, intent(in) :: b
        procedure(func1), pointer, intent(in) :: p
        func2 = p(b)
    end function
end module

program procInterface001c
use m
    class(AbstractParent), allocatable :: b1

    c1%pp1 => func1
    c1%pp2 => func2

    allocate(b1, SOURCE=Base(4))
    print *, c1%pp2(b1, c1%pp1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(5,6,null(),null()))
    print *, c1%pp2(b1, c1%pp1)
end