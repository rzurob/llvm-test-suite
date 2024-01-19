!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name. The procedure pointer
!                              is a dummy argument, and the associated
!                              procedure is given using a procedure
!                              pointer component. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              and interface implied by use association
!                              to declare interface-name. The actual
!                              procedure associated has different name
!                              from interface-name. The return value of
!                              function is array.
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

    interface
        function interfaceFunc1(b)
        import Base
            type(Base), intent(in) :: b(10)
            type(Base) :: interfaceFunc1(3,5)
        end function

        function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            type(Base), intent(in) :: b(10)
            procedure(interfaceFunc1), pointer, intent(in) :: p
            type(Base) :: interfaceFunc2(3,5)
        end function
    end interface
end module

module m2
use m1
    contains

    function func1(b)
        type(Base), intent(in) :: b(10)
        type(Base) :: func1(3,5)
        func1 = reshape(b,(/3,5/),(/Base(-1),Base(-2)/),(/2,1/))
    end function

    function func2(b, p)
        type(Base), intent(in) :: b(10)
        ! interface-name is explicit by use association
        procedure(interfaceFunc1), pointer, intent(in) :: p
        type(Base) :: func2(3,5)
        func2 = p(b)
    end function
end module

program interfaceName003e
use m2
    type Container
        procedure(interfaceFunc1), nopass, pointer :: pp1
        procedure(interfaceFunc2), nopass, pointer :: pp2
    end type

    type(Container) :: c1
    c1%pp1 => func1
    c1%pp2 => func2

    print *, c1%pp2((/(Base(i),i=1,10)/), c1%pp1)
    print *, shape(c1%pp2((/(Base(i),i=1,10)/), c1%pp1))
end
