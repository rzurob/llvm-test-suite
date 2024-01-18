! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer005.f
! %VERIFY: transfer005.out:transfer005.vf
! %STDIN:
! %STDOUT: transfer005.out
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
!*    Physical representation of result has longer length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The leading part has the same physical representation as
!*  SOURCE, and the remaining part is processor dependent.
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

program transfer005
use m
    if(.NOT. same_type_as(transfer(Base(10), Base1(1,2)), &
     Base1(3,4))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base(10), Base1(1,2)), &
     Base(3))) then
        error stop 2_4
    end if

    associate(name1=>transfer(Base(10), Base1(1,2)))
        print *, name1%i
    end associate
end
