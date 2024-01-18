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
!                              implied by use association to declare the
!                              interface-name. The actual procedure
!                              associated has the same name as
!                              interface-name. The container data entity
!                              containing the procedure pointers is
!                              either pointer or allocatable.
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
        integer :: func1(2,4)
        func1 = reshape((/(i,i=1,8)/), (/2,4/))
    end function

    function func2(b)
        type(Base) :: b
        type(Base) :: func2(3,5)
        func2 = reshape((/(Base(b%i+j),j=1,15)/),(/3,5/))
    end function
end module

module m3
use m2
    type Container
        procedure(func1), nopass, pointer :: pp1
        procedure(func2), nopass, pointer :: pp2
    end type
end module

program interfaceName002i
use m2
use m3
    type(Container), pointer :: c1
    type(Container), allocatable :: c2

    allocate(c1)
    c1%pp1 => func1
    c1%pp2 => func2

    print *, "func1", c1%pp1()
    print *, "func1", shape(c1%pp1())

    print *, "func2", c1%pp2(Base(5))
    print *, "func2", shape(c1%pp2(Base(5)))

    allocate(c2)
    c2 = c1

    print *, "func1", c2%pp1()
    print *, "func1", shape(c2%pp1())

    print *, "func2", c2%pp2(Base(5))
    print *, "func2", shape(c2%pp2(Base(5)))
end
