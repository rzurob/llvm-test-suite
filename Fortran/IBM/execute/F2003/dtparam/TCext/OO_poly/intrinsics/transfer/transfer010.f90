! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/transfer/transfer010.f
! opt variations: -ql -qreuse=self

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type Base1(k2,k3)    ! (4,4)
        integer, kind :: k2,k3
        integer(k2)      i
        integer(k3)      j
    end type
end module

program transfer010
use m
    type(Base(4)), pointer :: m1

    if(.NOT. same_type_as(transfer(Base1(4,4)(8,9), m1), &
     Base(4)(1))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base1(4,4)(8,9), m1), &
     Base1(4,4)(1,2))) then
        error stop 2_4
    end if

    associate(name1=>transfer(Base1(4,4)(8,9), m1))
        print *, name1
    end associate
end
