! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/component1/procInterface002d.f
! opt variations: -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
        procedure(), nopass, pointer :: spp
    end type
end module

program procInterface002d
use m
    type(Base(4)) :: b1
    b1 = Base(4)(10, null())

    b1%spp => sub1

    contains

    subroutine sub1()
        print *, "sub1"
    end subroutine
end
