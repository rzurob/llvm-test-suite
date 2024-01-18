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
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name. The actual procedure
!                              associated has the same name as
!                              interface-name.
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
use m2
    type Container
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type
end module

program interfaceName002h
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
