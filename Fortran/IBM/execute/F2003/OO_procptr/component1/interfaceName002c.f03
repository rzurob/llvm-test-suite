!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare a procedure pointer component
!                              using procedure declaration statement.
!                              specify procedure interface using
!                              interface-name, which is either a
!                              module subroutine or a module function
!                              Non-poly. Intrinsic or derived type,
!                              scalar.
!
!                              This test case use explicit interface to
!                              declare the interface-name. The actual
!                              procedure associated has different name
!                              from interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base
        integer i
    end type
end module

module m2
use m1
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

module m3
use m1
    interface
        subroutine interfaceSub1(i, b)
        use m1
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        real function interfaceFunc1()
        end function

        function interfaceFunc2(b)
        use m1
            type(Base) :: b
            type(Base) :: interfaceFunc2
        end function
    end interface

    type Container
        procedure(interfaceSub1), nopass, pointer :: pp1
        procedure(interfaceFunc1), nopass, pointer :: pp2
        procedure(interfaceFunc2), nopass, pointer :: pp3
    end type
end module

program interfaceName002c
use m2
use m3
    type(Container) :: c1
    c1%pp1 => sub1
    c1%pp2 => func1
    c1%pp3 => func2

    call c1%pp1(10, Base(11))
    print *, "func1", int(c1%pp2())
    print *, "func2", c1%pp3(Base(5))
end
