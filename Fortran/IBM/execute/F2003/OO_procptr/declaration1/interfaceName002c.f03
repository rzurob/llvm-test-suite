!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              module subroutine and function. The
!                              actual procedure associated has different
!                              name from interface-name.
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

    contains

    subroutine sub1(i, b)
        integer, intent(in) :: i
        type(Base), intent(in) :: b
        print *, "sub1", i, b
    end subroutine

    real function func1()
        func1 = 20
    end function

    function func2(b)
        type(Base) :: b
        type(Base) :: func2
        func2 = b
    end function
end module

program interfaceName002c
use m
    interface
        subroutine interfaceSub1(i, b)
        use m
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        real function interfaceFunc1()
        end function

        function interfaceFunc2(b)
        use m, only : Base
            type(Base) :: b
            type(Base) :: interfaceFunc2
        end function
    end interface

    procedure(interfaceSub1), pointer :: pp1
    procedure(interfaceFunc1), pointer :: pp2
    procedure(interfaceFunc2), pointer :: pp3

    pp1 => sub1
    pp2 => func1
    pp3 => func2

    call pp1(10, Base(11))
    print *, "func1", int(pp2())
    print *, "func2", pp3(Base(5))
end