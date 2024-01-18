!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an external function.
!                              Non-poly. Intrinsic or derived type,
!                              scalar.
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

program procInterface001a
use m
    interface
        integer function func1()
        end function

        function func2(b)
        use m
            type(Base) :: b
            type(Base) :: func2
        end function
    end interface

    procedure(integer), pointer :: pp1
    procedure(type(Base)), pointer :: pp2

    pp1 => func1
    pp2 => func2

    print *, "func1", pp1()
    print *, "func2", pp2(Base(5))
end

integer function func1()
    func1 = 20
end function

function func2(b)
use m
    type(Base) :: b
    type(Base) :: func2
    func2 = Base(b%i * 2)
end function
