! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv /tstdev/OO_poly/intrinsics/transfer/argumentKeyword001.f
! opt variations: -qnol -qdefaultpv

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Actual arguments are specified using argument keywords.
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

    type Base1(n2,k2)    ! (20,4)
        integer, kind     :: k2
        integer, len      :: n2
        type(Base(n2,k2)) :: b
    end type
end module

program argumentKeyword001
use m
    if(.NOT. same_type_as(transfer(SOURCE=Base(20,4)(10), &
     MOLD=Base1(20,4)(Base(20,4)(2))), Base1(20,4)(Base(20,4)(1)))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(MOLD=Base1(20,4)(Base(20,4)(2)), &
     SOURCE=Base(20,4)(10)), &
     Base(20,4)(1))) then
        error stop 2_4
    end if

    print *, transfer(Base(20,4)(10), Base1(20,4)(Base(20,4)(2)), 1)
    print *, transfer(SIZE=1, SOURCE=Base(20,4)(10), MOLD=Base1(20,4)(Base(20,4)(2)))
    print *, transfer(Base(20,4)(10), SIZE=1, MOLD=Base1(20,4)(Base(20,4)(2)))
end
