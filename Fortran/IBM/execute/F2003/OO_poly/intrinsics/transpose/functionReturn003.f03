!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : intrinsics/transpose
!* DATE                       : 12/31/2004
!* PRIMARY FUNCTIONS TESTED   : transpose
!* SECONDARY FUNCTIONS TESTED : finalization
!* DESCRIPTION                : Cross testing finalization.
!* ===================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 04/28/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Due to the cancellation of defect
!*                                 297603, this file was rewinded back
!*                                 to version 1. Now use defect 297792
!*                                 to restore the changes.
!*                              2) Verification file is also updated.
!*                              3) The TRUN header is removed.
!* ====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 1
        contains
        final :: finalizeBase,finalizeBaseRank1,finalizeBaseRank2
    end type

    type, extends(Base) :: Child
        integer :: j = 2
        contains
        final :: finalizeChild,finalizeChildRank1,finalizeChildRank2
    end type

    contains

    subroutine finalizeBase(b)
        type(Base), intent(inout) :: b
        print *, "B"
    end subroutine

    subroutine finalizeBaseRank1(b)
        type(Base), intent(inout) :: b(:)
        print *, "BX"
    end subroutine

    subroutine finalizeBaseRank2(b)
        type(Base), intent(inout) :: b(:,:)
        print *, "BXX"
    end subroutine

    subroutine finalizeChild(c)
        type(Child), intent(inout) :: c
        print *, "C"
    end subroutine

    subroutine finalizeChildRank1(c)
        type(Child), intent(inout) :: c(:)
        print *, "CX"
    end subroutine

    subroutine finalizeChildRank2(c)
        type(Child), intent(inout) :: c(:,:)
        print *, "CXX"
    end subroutine
end module

program functionReturn003
use m
    type(Child) :: b1(3,2)
    b1%i = reshape((/(i,i=1,6)/), (/3,2/))
    b1%j = reshape((/(i,i=-1,-6,-1)/), (/3,2/))

    print *, transpose(b1)
end
