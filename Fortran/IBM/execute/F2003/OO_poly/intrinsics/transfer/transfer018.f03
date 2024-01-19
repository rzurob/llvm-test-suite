! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is array
!*    Physical representation of result has shorter length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The physical representation is the leading part of that of
!*  SOURCE.
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
        integer k(2)
        type(Base) :: b
        integer i(6)
        integer j(3)
    end type
end module

program transfer018
use m
    type(Base1) :: b1
    type(Base1), pointer :: m1
    type(Base) :: src1(4,4)
    src1 = reshape((/(Base(i), i=3,8)/), (/4,4/), &
     (/Base(-1),Base(-2)/), (/2,1/))

    if(.NOT. same_type_as(transfer(src1, m1), b1)) error stop 1_4

    associate(name1=>transfer(src1, m1))
        print *, name1
    end associate
end
