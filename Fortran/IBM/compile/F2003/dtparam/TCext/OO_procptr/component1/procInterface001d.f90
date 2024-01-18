! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/component1/procInterface001d.f
! opt variations: -ql

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
        procedure(integer), nopass, pointer :: pp1
    end type
end module

program procInterface001d
use m
    type(Base(4)) :: b1
    b1 = Base(4)(10, null())

    b1%pp1 => func1

    contains

    integer function func1()
        func1 = 20
    end function
end
