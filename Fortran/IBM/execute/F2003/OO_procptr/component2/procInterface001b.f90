!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is a module function. Poly and
!                              unlimited poly. Intrinsic or derived
!                              type, scalar.
!
!                              Involves abstract type. Function returns
!                              derived type.
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

    contains

    function func1(b)
        class(AbstractParent), intent(in) :: b
        type(Base), allocatable :: func1
        select type (b)
            type is (Base)
                allocate(func1, SOURCE=Base(b%i*2))
            type is (Child)
                allocate(func1, SOURCE=Base(b%i+b%j))
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(*), allocatable, intent(in) :: b
        type(Base) :: func2
        select type (b)
            type is (Base)
                func2 = Base(b%i/2)
            type is (Child)
                func2 = Base(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end module

program procInterface001b
use m
    type(Base) :: rv1
    type(Child) :: c1
    class(AbstractParent), pointer :: b1
    class(*), allocatable :: b2

    c1%pp1 => func1
    c1%pp2 => func2

    allocate(b1, SOURCE=Child(4,5,null(),null()))
    rv1 = c1%pp1(b1)
    print *, "Func1", rv1
    deallocate(b1)

    allocate(b2, SOURCE=Base(6))
    rv1 = c1%pp2(b2)
    print *, "Func2", rv1
    deallocate(b2)
end
