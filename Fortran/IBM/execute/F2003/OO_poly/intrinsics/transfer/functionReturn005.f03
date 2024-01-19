!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* DATE                       : 12/30/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED : finalization
!* DESCRIPTION                : Cross testing with finalization.
!* =====================================================================
!* REVISION HISTORY
!*                    MM/DD/YY : 04/28/05
!*                        Init : yongdu@ca.ibm.com
!*                    Comments : 1) Due to the cancellation of defect
!*                                  297603, changes of this file was
!*                                  rewinded. Now use defect 297792 to
!*                                  restore those changes. Also removed
!*                                  the TRUN header. Note the vf is
!*                                  also restored.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer :: i = 1
        contains
        final :: finalizeBase,finalizeBaseRank1
    end type

    type, extends(Base) :: Child
        integer :: j = 2
        contains
        final :: finalizeChild,finalizeChildRank1
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

    subroutine finalizeChild(c)
        type(Child), intent(inout) :: c
        print *, "C"
    end subroutine

    subroutine finalizeChildRank1(c)
        type(Child), intent(inout) :: c(:)
        print *, "CX"
    end subroutine
end module

program functionReturn005
use m
    type(Base) :: b1(6)
    type(Child) :: c1(2)
    b1%i = (/ (i, i=1,6) /)

    print *, transfer(b1, c1)
end
