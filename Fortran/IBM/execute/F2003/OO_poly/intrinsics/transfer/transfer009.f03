! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is scalar
!*    Physical representation of result has shorter length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The physical representation is the leading part of that of
!*  SOURCE.
!*    Non-poly
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

program transfer009
use m
    if(.NOT. same_type_as(transfer(Base1(1,2), Base(12)), &
     Base(3))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base1(1,2), Base(12)), &
     Base1(1,2))) then
        error stop 2_4
    end if

    associate(name1=>transfer(Base1(8,9), Base(12)))
        print *, name1
    end associate
end
