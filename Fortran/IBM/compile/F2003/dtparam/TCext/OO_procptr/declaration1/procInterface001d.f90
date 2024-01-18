! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/procInterface001d.f
! opt variations: -qnol

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type
end module

program procInterface001d
use m
    procedure(integer), pointer :: pp1
    procedure(func2), pointer :: pp2

    pp1 => func1
    pp2 => func2

    print *, "func1", pp1()
    print *, "func2", pp2(Base(20,4)(5))

    contains

    integer function func1()
        func1 = 20
    end function

    function func2(b)
        type(Base(*,4)), intent(in) :: b
        type(Base(20,4)) :: func2
        func2 = Base(20,4)(b%i * 2)
    end function
end
