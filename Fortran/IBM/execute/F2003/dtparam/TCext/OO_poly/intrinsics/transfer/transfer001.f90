! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_poly/intrinsics/transfer/transfer001.f
! opt variations: -ql -qdefaultpv

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        type(Base(k2)) :: b
    end type
end module

program transfer001
use m
    if(.NOT. same_type_as(transfer(Base(4)(10), Base1(4)(Base(4)(2))), &
     Base1(4)(Base(4)(1)))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base(4)(10), Base1(4)(Base(4)(2))), &
     Base(4)(1))) then
        error stop 2_4
    end if

    print *, transfer(Base(4)(10), Base1(4)(Base(4)(2)))
end
