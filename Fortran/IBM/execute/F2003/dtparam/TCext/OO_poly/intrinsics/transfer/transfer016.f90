! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_poly/intrinsics/transfer/transfer016.f
! opt variations: -ql -qdefaultpv -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer016.f
! %VERIFY: transfer016.out:transfer016.vf
! %STDIN:
! %STDOUT: transfer016.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/16/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is array
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type Base1(k2,k3,k4)    ! (4,4,4)
        integer, kind  :: k2,k3,k4
        integer(k2)       k(2)
        type(Base(k2)) :: b
        integer(k3)       i(6)
        integer(k4)       j(3)
    end type
end module

program transfer016
use m
    type(Base1(4,4,4)) :: b1
    type(Base1(4,4,4)), pointer :: m1
    type(Base(4)) :: src1(3,3)
    src1 = reshape((/(Base(4)(i), i=2,10)/), (/3,3/), (/Base(4)(-1)/), (/2,1/))

    if(.NOT. same_type_as(transfer(src1, m1), b1)) error stop 1_4

    associate(name1=>transfer(src1, m1))
        print *, name1%k
        print *, name1%b
        print *, name1%i
    end associate
end
