!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to call
!                              external procedures. It also involves
!                              implicit typing. The actual procedure
!                              name has the same name as interface-name.
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

        integer function ifunc1()
        end function

        real function func2(b)
        import Base
            type(Base) :: b
        end function
    end interface
end module

program interfaceName001b
use m
    implicit real (i), type(Base) (f)
    procedure(sub1), pointer :: pp1
    procedure(ifunc1), pointer :: pp2
    procedure(func2), pointer :: pp3

    pp1 => sub1
    pp2 => ifunc1
    pp3 => func2

    call pp1(10, Base(11))
    print *, "ifunc1", pp2()
    print *, "func2", int(pp3(Base(5)))
end

subroutine sub1(i, b)
use m, only : Base
    integer, intent(in) :: i
    type(Base), intent(in) :: b
    print *, "sub1", i, b
end subroutine

integer function ifunc1()
    ifunc1 = 20
end function

real function func2(b)
use m, only : Base
    type(Base) :: b
    func2 = b%i
end function
