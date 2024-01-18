!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is an internal function.
!                              Non-poly. Intrinsic or derived type,
!                              scalar.
!
!                              This is a diagnostic test case.
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
        procedure(integer), nopass, pointer :: pp1
        procedure(type(Base)), nopass, pointer :: pp2
    end type
end module

program procInterface001d
use m
    type(Base) :: b1
    b1 = Base(10, null(), null())

    b1%pp1 => func1
    b1%pp2 => func2

    contains

    integer function func1()
        func1 = 20
    end function

    function func2(b)
        type(Base), intent(in) :: b
        type(Base) :: func2
        func2%i = b%i * 2
    end function
end
