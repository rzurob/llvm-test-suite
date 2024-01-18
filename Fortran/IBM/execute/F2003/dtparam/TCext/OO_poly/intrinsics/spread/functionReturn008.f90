! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/spread/functionReturn008.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : Cross testing with finalization.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 1
        contains
        final :: finalizeBase,finalizeBaseRank1,finalizeBaseRank2
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 2
        contains
        final :: finalizeChild,finalizeChildRank1,finalizeChildRank2
    end type

    contains

    subroutine finalizeBase(b)
        type(Base(4)), intent(inout) :: b
        print *, "B"
    end subroutine

    subroutine finalizeBaseRank1(b)
        type(Base(4)), intent(inout) :: b(:)
        print *, "BX"
    end subroutine

    subroutine finalizeBaseRank2(b)
        type(Base(4)), intent(inout) :: b(:,:)
        print *, "BXX"
    end subroutine

    subroutine finalizeChild(c)
        type(Child(4)), intent(inout) :: c
        print *, "C"
    end subroutine

    subroutine finalizeChildRank1(c)
        type(Child(4)), intent(inout) :: c(:)
        print *, "CX"
    end subroutine

    subroutine finalizeChildRank2(c)
        type(Child(4)), intent(inout) :: c(:,:)
        print *, "CXX"
    end subroutine
end module

program functionReturn008
use m
    type(Child(4)) :: c1(2)
    c1%i = (/(i, i=1,2)/)
    c1%j = (/(i, i=3,4)/)

    print *, spread(c1, 2, 3)
end
