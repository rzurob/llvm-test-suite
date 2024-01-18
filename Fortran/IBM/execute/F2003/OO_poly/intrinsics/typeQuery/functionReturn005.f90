! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : A or MOLD is the return value of an
!*                               external function call. Cross testing
!*                               with finalization.
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
    type Base
        integer i
        contains
        final :: finalizeBase
    end type

    type, extends(Base) :: Child
        character(10) :: c
        class(Base), allocatable :: b
        type(Base) :: b1
        contains
        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase(b)
        type(Base), intent(inout) :: b
        print *, "Base"
    end subroutine

    subroutine finalizeChild(c)
        type(Child), intent(inout) :: c
        print *, "Child"
    end subroutine

    function func1()
        class(Base), allocatable :: func1
        allocate(Base::func1)
    end function

    function func2()
        class(Child), allocatable :: func2
        allocate(Child::func2)
        allocate(Child::func2%b)
    end function
end module

program functionReturn005
use m
    type(Base) :: arg1

    if(.NOT. extends_type_of(func1(), arg1)) error stop 1_4
    if(.NOT. extends_type_of(func2(), arg1)) error stop 2_4

    if(.NOT. same_type_as(func1(), arg1)) error stop 3_4
    if(same_type_as(func2(), arg1)) error stop 4_4
end
