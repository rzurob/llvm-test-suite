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
!                              function is array, and is either pointer
!                              or allocatable. The dummy argument of
!                              function is array, and is either pointer
!                              or allocatable.
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
            type(Base), allocatable, intent(in) :: b(:)
            type(Base), pointer :: interfaceFunc1(:,:)
        end function

        function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            type(Base), allocatable, intent(in) :: b(:)
            procedure(interfaceFunc1), pointer, intent(in) :: p
            type(Base), pointer :: interfaceFunc2(:,:)
        end function
    end interface
end module

module m2
use m1
    contains

    function func1(b)
        type(Base), allocatable, intent(in) :: b(:)
        type(Base), pointer :: func1(:,:)
        allocate(func1(3,5))
        func1 = reshape(b,(/3,5/), (/Base(-1),Base(-2)/),(/2,1/))
    end function

    function func2(b, p)
        type(Base), allocatable, intent(in) :: b(:)
        ! interface-name is explicit by use association
        procedure(interfaceFunc1), pointer, intent(in) :: p
        type(Base), pointer :: func2(:,:)
        allocate(func2(3,5))
        func2 = p(b)
    end function
end module

program interfaceName003h
use m2
    type Container
        procedure(interfaceFunc1), nopass, pointer :: pp1
        procedure(interfaceFunc2), nopass, pointer :: pp2
    end type

    type(Base), allocatable :: b1(:)

    type(Container) :: c1
    c1%pp1 => func1
    c1%pp2 => func2

    allocate(b1(10))
    b1 = (/(Base(i),i=1,10)/)

    print *, c1%pp2(b1, c1%pp1)
    print *, shape(c1%pp2(b1, c1%pp1))
end
