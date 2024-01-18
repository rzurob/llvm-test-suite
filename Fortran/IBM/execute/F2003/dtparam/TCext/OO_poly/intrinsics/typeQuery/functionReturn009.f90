! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodeferredlp /tstdev/OO_poly/intrinsics/typeQuery/functionReturn009.f
! opt variations: -qnock -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/11/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Test that temporary object that is
!*    allocatable and is a function return shall be deallocated after
!*    execution of the innermost executable construct containing the
!*    function reference.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2,n2)    ! (20,4,1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type

    class(*), pointer :: b1 => null()

    contains

    function func1()
        class(Base(20,4)), allocatable, target :: func1
        allocate(func1)
        b1 => func1
    end function
end module

program functionReturn009
use m
    if(extends_type_of(b1, Base(20,4)(1))) error stop 1_4
    if(.NOT. extends_type_of(func1(), Base(20,4)(1)) .AND. &
       .NOT. extends_type_of(b1, Base(20,4)(1))) error stop 2_4

    b1 => null()
    if(same_type_as(b1, Base(20,4)(1))) error stop 3_4
    if(.NOT. same_type_as(func1(), Base(20,4)(1)) .AND. &
       .NOT. same_type_as(b1, Base(20,4)(1))) error stop 4_4
end
