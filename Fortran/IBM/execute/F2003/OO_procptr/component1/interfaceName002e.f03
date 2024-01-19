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
!                              This test case use explicit interface to
!                              declare interface-name. The actual
!                              procedure associated has different name
!                              from interface-name. The return value of
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
end module

module m2
use m1
    contains

    function func1()
        integer, pointer :: func1(:,:)
        allocate(func1(2,4))
        func1 = reshape((/(i,i=1,8)/), (/2,4/))
    end function

    function func2(b)
        type(Base) :: b
        type(Base), allocatable :: func2(:,:)
        allocate(func2(3,5))
        func2 = reshape((/(Base(b%i+j),j=1,15)/), (/3,5/))
    end function
end module

module m3
use m1
    interface
        function interfaceFunc1()
            integer, pointer :: interfaceFunc1(:,:)
        end function

        function interfaceFunc2(b)
        import Base
            type(Base) :: b
            type(Base), allocatable :: interfaceFunc2(:,:)
        end function
    end interface

    type Container
        procedure(interfaceFunc1), nopass, pointer :: pp1
        procedure(interfaceFunc2), nopass, pointer :: pp2
    end type
end module


program interfaceName002e
use m2
use m3
    type(Container) :: c1
    c1%pp1 => func1
    c1%pp2 => func2

    print *, "func1", c1%pp1()
    print *, "func1", shape(c1%pp1())

    print *, "func2", c1%pp2(Base(5))
    print *, "func2", shape(c1%pp2(Base(5)))
end
