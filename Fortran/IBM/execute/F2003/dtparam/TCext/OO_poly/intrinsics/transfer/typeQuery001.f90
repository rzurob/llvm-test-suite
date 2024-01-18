! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transfer/typeQuery001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/20/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Use EXTENDS_TYPE_OF and SAME_TYPE_AS to check the return value.
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
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 99
    end type
end module

program typeQuery001
use m
    class(*), pointer :: b1(:) => null()
    type(Base(:,4)), pointer :: b2(:) => null()
    class(Base(:,4)), pointer :: b3(:,:) => null()
    allocate(b1(10), SOURCE = (/ (Child(20,4)(i,i+100), i=1,10) /))
    allocate(b2(10), SOURCE = (/ (Base(20,4)(i), i=1,10) /))
    allocate(Child(20,4)::b3(3,3))

    if(.NOT. same_type_as(transfer(b1, b2), Base(20,4)(5))) then
        error stop 1_4
    end if

    if(.NOT. same_type_as(transfer(b2, b1), Child(20,4)(1,2))) then
        error stop 2_4
    end if

    if(.NOT. extends_type_of(b3, transfer(b1, b2))) then
        error stop 3_4
    end if
end
