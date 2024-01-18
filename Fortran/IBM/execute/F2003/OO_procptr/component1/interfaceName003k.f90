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
!                              procedure associated has the same name as
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
    end type

    contains

    subroutine sub1(i, b)
        ! b is implicit type(Base) by host association
        ! i is implicit integer by default implicit typing
        print *, "sub1", i, b
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

program interfaceName003k
use m
    implicit real (f,s), type(Container) (c)

    type Container
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type

    interface
        subroutine sub2(p1, p2, p3)
        use m
            procedure(sub1), pointer, intent(in):: p1
            procedure(func1), pointer, intent(in):: p2
            procedure(func2), pointer, intent(in):: p3
        end subroutine
    end interface

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
