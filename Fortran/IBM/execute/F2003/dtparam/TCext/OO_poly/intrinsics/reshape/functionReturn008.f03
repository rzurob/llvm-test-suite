! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/reshape/functionReturn008.f
! opt variations: -ql -qdefaultpv -qreuse=none

!*********************************************************************
!* ===================================================================
!* DATE                       : 11/02/2004
!* PRIMARY FUNCTIONS TESTED   : reshape
!* DESCRIPTION                : SOURCE is the return value of an
!*                              external function call. Cross testing
!*                              with finalization.
!* ===================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 04/05/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header.
!*                              2) Changed the source code to reduce
!*                                 the array size and add finalization
!*                                 for rank 1 and 2 arrays.
!*                              3) Updated verification file.
!* ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
        contains
        final :: finalizeBase, finalizeBase1, finalizeBase2
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
        class(Base(k1)), allocatable :: b
        type(Base(k1)) :: b1
        contains
        final :: finalizeChild, finalizeChild1, finalizeChild2
    end type

    contains

    subroutine finalizeBase(b)
        type(Base(4)), intent(inout) :: b
        print *, "Base"
    end subroutine

    subroutine finalizeBase1(b)
        type(Base(4)), intent(inout) :: b(:)
        print *, "Base1"
    end subroutine

    subroutine finalizeBase2(b)
        type(Base(4)), intent(inout) :: b(:,:)
        print *, "Base2"
    end subroutine

    subroutine finalizeChild(c)
        type(Child(4)), intent(inout) :: c
        print *, "Child"
    end subroutine

    subroutine finalizeChild1(c)
        type(Child(4)), intent(inout) :: c(:)
        print *, "Child1"
    end subroutine

    subroutine finalizeChild2(c)
        type(Child(4)), intent(inout) :: c(:,:)
        print *, "Child2"
    end subroutine

    function func1()
        class(Base(4)), allocatable :: func1(:)
        allocate(Child(4)::func1(7))
    end function
end module

program functionReturn008
use m
    class(Base(4)), allocatable :: arg1(:,:)

    allocate(arg1(2,2), SOURCE=reshape(func1(), (/2,2/)))
end
