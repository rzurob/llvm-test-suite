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
!                              scalar or array.
!
!                              This test case use explicit interface
!                              implied by module procedures to declare
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

module m
    type Base
        integer i
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type

    contains

    subroutine sub1(i, b)
        integer, intent(in) :: i
        type(Base), intent(in) :: b
        print *, "sub1", i, b%i
    end subroutine

    integer function func1()
        func1 = 20
    end function

    function func2(b)
        type(Base) :: b
        type(Base) :: func2(3)
        func2%i = b%i
    end function
end module

program interfaceName002a
use m
    type(Base) :: b1
    type(Base) :: b2

    b1%pp1 => sub1
    b1%pp2 => func1
    b1%pp3 => func2

    b2%i = 11

    call b1%pp1(10, b2)
    print *, "func1", b1%pp2()
    b2%i = 5
    associate(name1=>b1%pp3(b2))
        print *, "func2", name1%i
    end associate
end
