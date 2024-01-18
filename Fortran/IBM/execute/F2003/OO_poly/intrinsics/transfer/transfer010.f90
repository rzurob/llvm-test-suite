! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer010.f
! %VERIFY: transfer010.out:transfer010.vf
! %STDIN:
! %STDOUT: transfer010.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  ORIGIN                     :
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

program transfer010
use m
    type(Base), pointer :: m1

    if(.NOT. same_type_as(transfer(Base1(8,9), m1), &
     Base(1))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base1(8,9), m1), &
     Base1(1,2))) then
        error stop 2_4
    end if

    associate(name1=>transfer(Base1(8,9), m1))
        print *, name1
    end associate
end
