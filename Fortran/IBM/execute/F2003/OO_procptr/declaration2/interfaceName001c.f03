!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              actual procedure associated has the same
!                              name as interface-name. The dummy
!                              arguments are pointer.
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

program interfaceName001c
use m
    interface
        subroutine sub1(b)
        use m
            class(Base), pointer, intent(in) :: b
        end subroutine

        function func1(b)
        use m
            class(Base), pointer, intent(in) :: b
            class(Base), pointer :: func1
        end function
    end interface

    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    class(Base), pointer :: b1

    pp1 => sub1
    pp2 => func1

    allocate(b1, SOURCE=Base(10))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,22))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end

subroutine sub1(b)
use m
    class(Base), pointer, intent(in) :: b
    select type (b)
        type is (Base)
            print *, "sub1 Base", b
        type is (Child)
            print *, "sub1 Child", b
        class default
            error stop 3_4
    end select
end subroutine

function func1(b)
use m
    class(Base), pointer, intent(in) :: b
    class(Base), pointer :: func1
    select type (b)
        type is (Base)
            allocate(func1, SOURCE=Base(b%i*2))
        type is (Child)
            allocate(func1, SOURCE=Child(b%j,b%i))
        class default
            error stop 4_4
    end select
end function