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
!                              This test case uses explicit interface
!                              and interface implied by use association
!                              to declare interface-name. The actual
!                              procedure associated has the same name as
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
    end type

    contains

    integer function func1(b)
        type(Base), intent(in) :: b
        func1 = b%i
    end function
end module

program interfaceName003a
use m
    interface
        subroutine sub1(i, b, p)
        use m
            integer, intent(in) :: i
            type(Base), intent(in) :: b
            procedure(func1), pointer, intent(in):: p
        end subroutine
    end interface

    type Container
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
    end type

    type(Container) :: c1

    c1%pp1 => sub1
    c1%pp2 => func1

    call c1%pp1(10, Base(11), c1%pp2)
end

subroutine sub1(i, b, p)
use m
    integer, intent(in) :: i
    type(Base), intent(in) :: b
    procedure(func1), pointer, intent(in):: p

    if(associated(p)) then
        print *, "sub1", p(b)
    else
        error stop 1_4
    end if
end subroutine