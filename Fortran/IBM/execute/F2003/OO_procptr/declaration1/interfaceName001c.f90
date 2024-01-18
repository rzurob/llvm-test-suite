!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
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
        function func1(b)
        import Base
            implicit integer (b), type(Base) (f)
        end function
    end interface
end module

program interfaceName001c
use m
    implicit integer (b), type(Base) (f)

    procedure(func1), pointer :: pp1
    pp1 => func1
    b1 = 5
    print *, "func1", pp1(b1)
end

function func1(b)
use m, only : Base
    implicit integer (b), type(Base) (f)
    func1%i = b
end function
