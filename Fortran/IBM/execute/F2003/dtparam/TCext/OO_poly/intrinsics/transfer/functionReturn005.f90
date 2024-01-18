! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/transfer/functionReturn005.f
! opt variations: -qnol -qreuse=base

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST BUCKET                : intrinsics/transfer
!* PROGRAMMER                 : Yong Du
!* DATE                       : 12/30/2004
!* PRIMARY FUNCTIONS TESTED   : transfer
!* SECONDARY FUNCTIONS TESTED : finalization
!* DRIVER STANZA              : xlf90
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 1
        contains
        final :: finalizeBase,finalizeBaseRank1
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: j = 2
        contains
        final :: finalizeChild,finalizeChildRank1
    end type

    contains

    subroutine finalizeBase(b)
        type(Base(*,4)), intent(inout) :: b
        print *, "B"
    end subroutine

    subroutine finalizeBaseRank1(b)
        type(Base(*,4)), intent(inout) :: b(:)
        print *, "BX"
    end subroutine

    subroutine finalizeChild(c)
        type(Child(*,4,*,4)), intent(inout) :: c
        print *, "C"
    end subroutine

    subroutine finalizeChildRank1(c)
        type(Child(*,4,*,4)), intent(inout) :: c(:)
        print *, "CX"
    end subroutine
end module

program functionReturn005
use m
    type(Base(20,4)) :: b1(6)
    type(Child(20,4,20,4)) :: c1(2)
    b1%i = (/ (i, i=1,6) /)

    print *, transfer(b1, c1)
end
