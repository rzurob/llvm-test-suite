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
!                              implied by module procedures to declare
!                              interface-name. The actual procedure
!                              associated has the same name as
!                              interface-name. It checks whether
!                              implicit typing affects interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    implicit real (f,s), type(Base) (b)

    type Base
        integer i
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type

    contains

    subroutine sub1(i, b)
        ! b is implicit type(Base) by host association
        ! i is implicit integer by default implicit typing
        print *, "sub1", i, b%i
    end subroutine

    function func1()
        ! redefine implicit typing rule for f
        implicit integer (f)
        func1 = 20
    end function

    function func2(b)
        ! b is implicit type(Base) by host association
        ! redefine implicit typing rule for f
        implicit type(Base) (f)
        func2%i = b%i
    end function
end module

program interfaceName002b
use m
    implicit real (f,s), type(Base) (b)

    b1%pp1 => sub1
    b1%pp2 => func1
    b1%pp3 => func2

    call b1%pp1(10, Base(11,null(),null(),null()))
    print *, "func1", b1%pp2()
    associate(name1=>b1%pp3(Base(5,null(),null(),null())))
        print *, "func2", name1%i
    end associate
end
