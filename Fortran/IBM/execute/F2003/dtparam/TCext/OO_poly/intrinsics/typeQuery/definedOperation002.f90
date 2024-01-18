! GB DTP extension using:
! ftcx_dtp -qck -qnol -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/definedOperation002.f
! opt variations: -qnock -ql -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/10/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Use defined operation to change the
!*    dynamic types.
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
end module

program definedOperation002
use m
    interface operator(+)
        type(Child(4,1,10)) function bPlusB2c(a, b)
        use m
            class(Base(4)), intent(in) :: a, b
        end function
    end interface

    interface operator(-)
        type(Base(4)) function cMinusC2b(a, b)
        use m
            class(Child(4,1,*)), intent(in) :: a, b
        end function
    end interface

    class(*), pointer :: u1 => null()
    type(Base(4)) :: b1
    class(Child(4,1,:)), allocatable :: c1
    type(Child(4,1,10)) :: c2 = Child(4,1,10)(1, "abc")
    b1%i = 3
    allocate(c1, SOURCE=Child(4,1,10)(4, "aaa"))

    allocate(u1, SOURCE=(b1+c1-c2))

    if(.NOT. extends_type_of(b1, u1)) error stop 1_4
    if(.NOT. extends_type_of(c1, u1)) error stop 2_4
    if(.NOT. extends_type_of(Base(4)(1), u1)) error stop 3_4

    if(.NOT. same_type_as(u1, b1)) error stop 4_4
    if(same_type_as(u1, c1)) error stop 5_4
    if(.NOT. same_type_as(u1, Base(4)(1))) error stop 6_4

    select type(name=>u1)
        type is (Base(4))
            if(name%i /= 6) error stop 7_4
        class default
            error stop 8_4
    end select
end

type(Child(4,1,10)) function bPlusB2c(a, b)
use m
    class(Base(4)), intent(in) :: a, b
    bPlusB2c%i = a%i + b%i
    bPlusB2c%c = "bPlusB2c"
end function

type(Base(4)) function cMinusC2b(a, b)
use m
    class(Child(4,1,*)), intent(in) :: a, b
    cMinusC2b%i = a%i - b%i
end function
