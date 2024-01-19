! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/11/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Test INTENT(OUT).
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    contains

    subroutine func1(a)
        class(*), allocatable, INTENT(OUT) :: a
        if(extends_type_of(a, Base(1))) error stop 1_4
        if(same_type_as(Base(1), a)) error stop 2_4
        if(.NOT. allocated(a)) allocate(Child::a)
    end subroutine
end module

program functionReturn010
use m
    class(*), allocatable :: b1
    allocate(Base::b1)

    if(.NOT. extends_type_of(Base(1), b1)) error stop 3_4
    if(.NOT. same_type_as(Base(1), b1)) error stop 4_4

    call func1(b1)

    if(extends_type_of(Base(1), b1)) error stop 5_4
    if(.NOT. extends_type_of(b1, Base(1))) error stop 6_4
    if(same_type_as(Base(1), b1)) error stop 7_4
    if(.NOT. same_type_as(Child(1,"abc"), b1)) error stop 8_4
end
