!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare a procedure pointer component
!                              using procedure declaration statement.
!                              specify procedure interface using
!                              interface-name, which is either an
!                              external subroutine or an external
!                              function. Non-poly. Intrinsic or derived
!                              type, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
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

    interface
        subroutine sub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        integer function func1()
        end function

        function func2(b)
        import Base
            type(Base) :: b
            type(Base) :: func2(5)
        end function
    end interface
end module

program interfaceName001d
use m
    interface
        subroutine interfaceSub1(i, b)
        use m
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        integer function interfaceFunc1()
        end function

        function interfaceFunc2(b)
        use m
            type(Base) :: b
            type(Base) :: interfaceFunc2(5)
        end function
    end interface

    type Container
        procedure(interfaceSub1), nopass, pointer :: pp1
        procedure(interfaceFunc1), nopass, pointer :: pp2
        procedure(interfaceFunc2), nopass, pointer :: pp3
    end type

    type(Container) :: c1

    c1%pp1 => sub1
    c1%pp2 => func1
    c1%pp3 => func2

    call c1%pp1(10, Base(11))
    print *, "func1", c1%pp2()
    print *, "func2", c1%pp3(Base(5))
end

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