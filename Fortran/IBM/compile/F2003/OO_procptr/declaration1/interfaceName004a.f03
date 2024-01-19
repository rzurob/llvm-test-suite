!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an internal subroutine or
!                              an internal function. Non-poly. Intrinsic
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
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2

    pp1 => sub1
    pp2 => func1

    contains

    subroutine sub1(i, b)
    use m
        integer, intent(in) :: i
        type(Base), intent(in) :: b
        print *, "sub1", i, b
    end subroutine

    integer function func1()
        func1 = 20
    end function
end
