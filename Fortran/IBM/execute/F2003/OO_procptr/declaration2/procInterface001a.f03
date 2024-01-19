!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an external function.
!                              Poly and unlimited poly. Intrinsic or
!                              derived type, scalar.
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

program procInterface001a
use m
    interface
        integer function ifunc1(b)
        use m
            class(Base), intent(in) :: b
        end function

        real function func2(b)
        use m
            class(*), pointer :: b
        end function
    end interface

!    procedure(integer), pointer :: pp1
!    procedure(real), pointer :: pp2

    procedure(ifunc1), pointer :: pp1
    procedure(func2), pointer :: pp2

    class(*), pointer :: b1

    pp1 => ifunc1
    pp2 => func2

    print *, "ifunc1", pp1(Child(4, 5))
    allocate(b1, SOURCE=Base(6))
    print *, "func2", int(pp2(b1))
end

integer function ifunc1(b)
use m
    class(Base), intent(in) :: b
    select type(b)
        type is (Base)
            ifunc1 = b%i * 2
        type is (Child)
            ifunc1 = b%i + b%j
        class default
            error stop 1_4
    end select
end function

real function func2(b)
use m
    class(*), pointer :: b
    select type(b)
        type is (Base)
            func2 = b%i / 2
        type is (Child)
            func2 = b%i - b%j
        class default
            error stop 2_4
    end select
end function
