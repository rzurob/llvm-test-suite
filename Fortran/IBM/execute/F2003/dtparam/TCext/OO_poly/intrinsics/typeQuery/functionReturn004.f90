! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/OO_poly/intrinsics/typeQuery/functionReturn004.f
! opt variations: -qnock -ql

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : A or MOLD is the return value of
!*                               intrinsic function transfer() and
!*                               transpose(). Cross testing with
!*                               select type.
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
        integer(k1)   :: i = 10
    end type

    type, extends(Base) :: Child(k2,n1)    ! (4,1,10)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: c = "aaa"
    end type
end module

program functionReturn004
use m
    type(Base(4)) :: b1, b2, b3
    type(Child(4,1,10)) :: c1, c2, c3
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2
    type(Base(4)) :: b4(2, 3)

    allocate(Complex::arg1)
    allocate(arg2, SOURCE=transfer((/1.0,2.0,3.0/), (0.0,0.0)))
    if(extends_type_of(arg1, arg2)) error stop 1_4
    if(.NOT. same_type_as(arg1, arg2)) error stop 2_4
    select type (arg2)
        type is (complex)
            if(arg2 .NE. (1.0,2.0)) error stop 3_4
        class default
            error stop 4_4
    end select

    deallocate(arg1, arg2)
    allocate(Base(4)::arg1)
    allocate(arg2, SOURCE=transfer((/b1,b2,b3/), Child(4,1,10)(1, "abc")))
    if(extends_type_of(arg1, arg2)) error stop 5_4
    if(.NOT. extends_type_of(arg2, arg1)) error stop 6_4
    if(same_type_as(arg1, arg2)) error stop 7_4
    select type (arg2)
        type is (Child(4,1,*))
            if(arg2%i .NE. 10) error stop 8_4
        class default
            error stop 9_4
    end select

    deallocate(arg2)
    allocate(arg2, SOURCE=transfer((/c1,c2,c3/), Base(4)(1)))
    if(.NOT. extends_type_of(arg1, arg2)) error stop 10_4
    if(.NOT. extends_type_of(arg2, arg1)) error stop 11_4
    if(.NOT. same_type_as(arg1, arg2)) error stop 12_4
    select type (arg2)
        type is (Base(4))
            if(arg2%i .NE. 10) error stop 13_4
        class default
            error stop 14_4
    end select

    if(.NOT. extends_type_of(b4, transpose(b4))) error stop 15_4
    if(.NOT. same_type_as(b4, transpose(b4))) error stop 16_4
end
