! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/definedOperation001.f
! opt variations: -qnock -qnol -qnodeferredlp

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
end module

program definedOperation001
use m
    interface operator(+)
        type(Child(20,4,1,10)) function bPlusB2c(a, b)
        use m
            class(Base(*,4)), intent(in) :: a, b
        end function
    end interface

    class(*), pointer :: u1 => null()
    type(Base(20,4)) :: b1
    class(Child(:,4,1,:)), allocatable :: c1
    b1%i = 1
    allocate(c1, SOURCE=Child(20,4,1,10)(2, "abc"))

    allocate(u1, SOURCE=(b1+c1))

    if(.NOT. extends_type_of(u1, b1)) error stop 1_4
    if(.NOT. extends_type_of(u1, c1)) error stop 2_4
    if(.NOT. extends_type_of(u1, Base(20,4)(1))) error stop 3_4

    if(same_type_as(u1, b1)) error stop 4_4
    if(.NOT. same_type_as(u1, c1)) error stop 5_4
    if(.NOT. same_type_as(u1, Child(20,4,1,10)(1, "a"))) error stop 6_4

    select type(name=>u1)
        type is (Child(*,4,1,*))
            if(name%i /= 3 .OR. name%c /= "bPlusB2c") error stop 7_4
        class default
            error stop 8_4
    end select
end

type(Child(20,4,1,10)) function bPlusB2c(a, b)
use m
    class(Base(*,4)), intent(in) :: a, b
    bPlusB2c%i = a%i + b%i
    bPlusB2c%c = "bPlusB2c"
end function
