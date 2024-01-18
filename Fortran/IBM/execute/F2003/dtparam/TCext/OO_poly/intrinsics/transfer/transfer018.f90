! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self /tstdev/OO_poly/intrinsics/transfer/transfer018.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer018.f
! %VERIFY: transfer018.out:transfer018.vf
! %STDIN:
! %STDOUT: transfer018.out
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       k(2)
        type(Base(k2)) :: b
        integer(k2)       i(6)
        integer(k2)       j(3)
    end type
end module

program transfer018
use m
    type(Base1(4)) :: b1
    type(Base1(4)), pointer :: m1
    type(Base(4)) :: src1(4,4)
    src1 = reshape((/(Base(4)(i), i=3,8)/), (/4,4/), &
     (/Base(4)(-1),Base(4)(-2)/), (/2,1/))

    if(.NOT. same_type_as(transfer(src1, m1), b1)) error stop 1_4

    associate(name1=>transfer(src1, m1))
        print *, name1
    end associate
end
