!*********************************************************************
!* =================================================================== 
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!* =================================================================== 
!* TEST CASE TITLE            : intrinsics/reshape/functionReturn008.f
!* PROGRAMMER                 : Yong Du
!* DATE                       : 11/02/2004
!* PRIMARY FUNCTIONS TESTED   : reshape
!* DRIVER STANZA              : xlf90
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
    type Base
        integer i
        contains
        final :: finalizeBase, finalizeBase1, finalizeBase2
    end type

    type, extends(Base) :: Child
        integer j
        class(Base), allocatable :: b
        type(Base) :: b1
        contains
        final :: finalizeChild, finalizeChild1, finalizeChild2
    end type

    contains

    subroutine finalizeBase(b)
        type(Base), intent(inout) :: b
        print *, "Base"
    end subroutine

    subroutine finalizeBase1(b)
        type(Base), intent(inout) :: b(:)
        print *, "Base1"
    end subroutine

    subroutine finalizeBase2(b)
        type(Base), intent(inout) :: b(:,:)
        print *, "Base2"
    end subroutine

    subroutine finalizeChild(c)
        type(Child), intent(inout) :: c
        print *, "Child"
    end subroutine

    subroutine finalizeChild1(c)
        type(Child), intent(inout) :: c(:)
        print *, "Child1"
    end subroutine

    subroutine finalizeChild2(c)
        type(Child), intent(inout) :: c(:,:)
        print *, "Child2"
    end subroutine

    function func1()
        class(Base), allocatable :: func1(:)
        allocate(Child::func1(7))
    end function
end module

program functionReturn008
use m
    class(Base), allocatable :: arg1(:,:)

    allocate(arg1(2,2), SOURCE=reshape(func1(), (/2,2/)))
end
