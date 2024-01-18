!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare a procedure pointer component
!                              using procedure declaration statement.
!                              Specify procedure interface using
!                              interface-name. The associated procedure
!                              is either an internal subroutine or an
!                              internal function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case is diagnostic.
!                              The actual procedure associated has the
!                              same name as interface-name.
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
end module

program interfaceName004a
use m
    type Container
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type

    type(Container) :: b1

    b1%pp1 => sub1
    b1%pp2 => func1
    b1%pp3 => func2

    contains

    subroutine sub1(i, b)
    use m, only : Base
        integer, intent(in) :: i
        type(Base), intent(in) :: b
        print *, "sub1", i, b
    end subroutine

    integer function func1()
        func1 = 20
    end function

    function func2(b)
    use m, only : Base
        type(Base) :: b
        type(Base) :: func2(5)
        func2 = b
    end function
end
