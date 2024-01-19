!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an internal function. Poly
!                              and unlimited poly. Intrinsic or derived
!                              type, scalar.
!
!                              This is a diagnostic test case.
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
    end type

    type, extends(Base) :: Child
        integer j
    end type

    interface
      type(Base) function mfunc2(b)
        import Base
        class(*), allocatable, intent(in) :: b

      end function
    end interface

    type Container
        procedure(integer), pointer, nopass :: pp1
        procedure(mfunc2), pointer, nopass :: pp2
    end type

    class(Container), pointer :: c1
end module

program procInterface001e
use m
    class(Base), pointer :: b1
    class(*), allocatable :: b2

    allocate(Container::c1)
    c1%pp1 => func1
    c1%pp2 => func2

    allocate(b1, SOURCE=Child(3,4))
    print *, "func1", c1%pp1(b1)

    allocate(b2, SOURCE=Base(5))
    print *, "func2", c1%pp2(b2)

    contains

    integer function func1(b)
        class(Base), intent(in) :: b
        select type (b)
            type is (Base)
                func1 = b%i
            type is (Child)
                func1 = b%i+b%j
            class default
                error stop 1_4
        end select
    end function

    type(Base) function func2(b)
        class(*), allocatable, intent(in) :: b
        select type (b)
            type is (Base)
                func2 = Base(-b%i)
            type is (Child)
                func2 = Base(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end
