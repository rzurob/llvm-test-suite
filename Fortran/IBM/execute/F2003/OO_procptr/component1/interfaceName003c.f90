!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name. The procedure pointer
!                              is a dummy argument, and the associated
!                              procedure is given using a procedure
!                              pointer component. Non-poly. Intrinsic
!                              or derived type, scalar.
!
!                              This test case use explicit interface
!                              and interface implied by use association
!                              to declare interface-name. The actual
!                              procedure associated has different name
!                              from interface-name.
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
        subroutine interfaceSub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        integer function interfaceFunc1()
        end function

        function interfaceFunc2(b)
        import Base
            type(Base), intent(in) :: b
            type(Base) :: interfaceFunc2
        end function
    end interface

    contains

    subroutine sub1(i, b)
        integer, intent(in) :: i
        type(Base), intent(in) :: b
        print *, "sub1", i, b
    end subroutine

    integer function func1()
        func1 = 20
    end function

    function func2(b)
        type(Base), intent(in) :: b
        type(Base) :: func2
        func2%i = b%i
    end function
end module

program interfaceName003c
use m
    type Container
        procedure(interfaceSub1), nopass, pointer :: pp1
        procedure(interfaceFunc1), nopass, pointer :: pp2
        procedure(interfaceFunc2), nopass, pointer :: pp3
    end type

    interface
        subroutine sub2(p1, p2, p3)
        use m
            procedure(interfaceSub1), pointer, intent(in):: p1
            procedure(interfaceFunc1), pointer, intent(in):: p2
            procedure(interfaceFunc2), pointer, intent(in):: p3
        end subroutine
    end interface

    type(Container) :: c1
    procedure(sub2), pointer :: p0
    p0 => sub2

    c1%pp1 => sub1
    c1%pp2 => func1
    c1%pp3 => func2

    call p0(c1%pp1, c1%pp2, c1%pp3)
end

subroutine sub2(p1, p2, p3)
use m
    procedure(sub1), pointer, intent(in):: p1
    procedure(func1), pointer, intent(in):: p2
    procedure(func2), pointer, intent(in):: p3

    if(associated(p1)) then
        call p1(10, Base(11))
    else
        error stop 1_4
    end if

    if(associated(p2)) then
        print *, "func1", p2()
    else
        error stop 2_4
    end if

    if(associated(p3)) then
        print *, "func2", p3(Base(5))
    else
        error stop 3_4
    end if
end subroutine
