! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/OO_poly/intrinsics/typeQuery/functionReturn010.f
! opt variations: -qnock -ql

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2,n1)    ! (4,1,10)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: c
    end type

    contains

    subroutine func1(a)
        class(*), allocatable, INTENT(OUT) :: a
        if(extends_type_of(a, Base(4)(1))) error stop 1_4
        if(same_type_as(Base(4)(1), a)) error stop 2_4
        if(.NOT. allocated(a)) allocate(Child(4,1,10)::a)
    end subroutine
end module

program functionReturn010
use m
    class(*), allocatable :: b1
    allocate(Base(4)::b1)

    if(.NOT. extends_type_of(Base(4)(1), b1)) error stop 3_4
    if(.NOT. same_type_as(Base(4)(1), b1)) error stop 4_4

    call func1(b1)

    if(extends_type_of(Base(4)(1), b1)) error stop 5_4
    if(.NOT. extends_type_of(b1, Base(4)(1))) error stop 6_4
    if(same_type_as(Base(4)(1), b1)) error stop 7_4
    if(.NOT. same_type_as(Child(4,1,10)(1,"abc"), b1)) error stop 8_4
end
