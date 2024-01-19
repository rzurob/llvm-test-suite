! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/OO_poly/intrinsics/typeQuery/functionReturn003.f
! opt variations: -qnock -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : A or MOLD is the return value of
!*                               intrinsic function transfer().
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

program functionReturn003
use m
    type(Base(20,4)) :: b1, b2, b3
    type(Child(20,4,1,10)) :: c1, c2, c3
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2
    allocate(Complex::arg1)

    allocate(arg2, SOURCE=transfer((/1.0,2.0,3.0/), (0.0,0.0)))
    if(extends_type_of(arg1, arg2)) error stop 1_4
    if(.NOT. same_type_as(arg1, arg2)) error stop 2_4

    deallocate(arg1, arg2)
    allocate(Base(20,4)::arg1)
    allocate(arg2, SOURCE=transfer((/b1,b2,b3/), Child(20,4,1,10)(1, "abc")))
    if(extends_type_of(arg1, arg2)) error stop 3_4
    if(.NOT. extends_type_of(arg2, arg1)) error stop 4_4
    if(same_type_as(arg1, arg2)) error stop 5_4

    deallocate(arg2)
    allocate(arg2, SOURCE=transfer((/c1,c2,c3/), Base(20,4)(1)))
    if(.NOT. extends_type_of(arg1, arg2)) error stop 6_4
    if(.NOT. extends_type_of(arg2, arg1)) error stop 7_4
    if(.NOT. same_type_as(arg1, arg2)) error stop 8_4
end
