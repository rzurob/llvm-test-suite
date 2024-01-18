!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated procedure is either an
!                              internal function or an internal
!                              subroutine. Non-poly. Intrinsic or
!                              derived type, scalar.
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
    end type
end module

program procInterface002d
use m
    procedure(), pointer :: spp
    procedure(), pointer :: ipp

    spp => sub1
    ipp => ifunc1

    contains

    subroutine sub1()
        print *, "sub1"
    end subroutine

    integer function ifunc1()
        ifunc1 = 10
    end function
end
