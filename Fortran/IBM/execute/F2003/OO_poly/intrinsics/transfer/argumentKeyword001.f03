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
    type Base
        integer i
    end type

    type Base1
        type(Base) :: b
    end type
end module

program argumentKeyword001
use m
    if(.NOT. same_type_as(transfer(SOURCE=Base(10), &
     MOLD=Base1(Base(2))), Base1(Base(1)))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(MOLD=Base1(Base(2)), &
     SOURCE=Base(10)), &
     Base(1))) then
        error stop 2_4
    end if

    print *, transfer(Base(10), Base1(Base(2)), 1)
    print *, transfer(SIZE=1, SOURCE=Base(10), MOLD=Base1(Base(2)))
    print *, transfer(Base(10), SIZE=1, MOLD=Base1(Base(2)))
end