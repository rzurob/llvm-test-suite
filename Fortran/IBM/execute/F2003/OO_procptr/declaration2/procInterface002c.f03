!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Do not specify proc-interface. The
!                              procedure pointer is a dummy argument.
!                              The associated function is a module
!                              function. Poly and unlimited poly.
!                              Intrinsic or derived type, scalar or
!                              array.
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
    end type

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
        implicit type(Base) (p)
        class(AbstractParent), allocatable, intent(in) :: b
        procedure(func1), pointer, intent(in) :: p
        func2 = p(b)
    end function
end module

program procInterface002c
use m
    implicit type(Base) (p)

    procedure(func1), pointer :: pp1
    procedure(func2), pointer :: pp2
    class(AbstractParent), allocatable :: b1

    pp1 => func1
    pp2 => func2

    allocate(b1, SOURCE=Base(4))
    print *, pp2(b1, pp1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(5,6))
    print *, pp2(b1, pp1)
end
