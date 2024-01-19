! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is scalar
!*    Physical representation of result has longer length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The leading part has the same physical representation as
!*  SOURCE, and the remaining part is processor dependent.
!*    Non-poly and MOLD can be an undefined variable.
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
        integer i
        integer j
    end type
end module

program transfer006
use m
    type(Base1), pointer :: m1

    if(.NOT. same_type_as(transfer(Base(10), m1), &
     Base1(1,2))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base(10), m1), &
     Base(1))) then
        error stop 2_4
    end if

    associate(name1=>transfer(Base(10), m1))
        print *, name1%i
    end associate
end
