! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer001.f
! %VERIFY: transfer001.out:transfer001.vf
! %STDIN:
! %STDOUT: transfer001.out
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
!*    Physical representation of result has the same length as that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD, and has the same physical representation as SOURCE.
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
        type(Base) :: b
    end type
end module

program transfer001
use m
    if(.NOT. same_type_as(transfer(Base(10), Base1(Base(2))), &
     Base1(Base(1)))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base(10), Base1(Base(2))), &
     Base(1))) then
        error stop 2_4
    end if

    print *, transfer(Base(10), Base1(Base(2)))
end
