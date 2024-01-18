! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/procInterface002d.f
! opt variations: -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
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
