! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/functionReturn005.f
! opt variations: -qnock -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
        contains
        final :: finalizeBase
    end type

    type, extends(Base) :: Child(k2,n2)    ! (20,4,1,10)
        integer, kind                  :: k2
        integer, len                   :: n2
        character(kind=k2,len=n2)      :: c
        class(Base(:,k1)), allocatable :: b
        type(Base(n1,k1))              :: b1
        contains
        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase(b)
        type(Base(*,4)), intent(inout) :: b
        print *, "Base"
    end subroutine

    subroutine finalizeChild(c)
        type(Child(*,4,1,*)), intent(inout) :: c
        print *, "Child"
    end subroutine

    function func1()
        class(Base(:,4)), allocatable :: func1
        allocate(Base(20,4)::func1)
    end function

    function func2()
        class(Child(:,4,1,:)), allocatable :: func2
        allocate(Child(20,4,1,10)::func2)
        allocate(Child(20,4,1,10)::func2%b)
    end function
end module

program functionReturn005
use m
    type(Base(20,4)) :: arg1

    if(.NOT. extends_type_of(func1(), arg1)) error stop 1_4
    if(.NOT. extends_type_of(func2(), arg1)) error stop 2_4

    if(.NOT. same_type_as(func1(), arg1)) error stop 3_4
    if(same_type_as(func2(), arg1)) error stop 4_4
end
