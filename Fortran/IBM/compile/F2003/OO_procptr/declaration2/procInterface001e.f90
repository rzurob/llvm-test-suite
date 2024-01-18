!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
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
end module

program procInterface001e
use m
    !procedure(integer), pointer :: pp1
     procedure(func1), pointer :: pp1
    !procedure(type(Base)), pointer :: pp2
     procedure(func2), pointer :: pp2
    class(Base), pointer :: b1
    class(*), allocatable :: b2

    pp1 => func1
    pp2 => func2

    allocate(b1, SOURCE=Child(3,4))
    print *, "func1", pp1(b1)

    allocate(b2, SOURCE=Base(5))
    print *, "func2", pp2(b2)

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
