! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/OO_poly/intrinsics/transfer/transfer009.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer009.f
! %VERIFY: transfer009.out:transfer009.vf
! %STDIN:
! %STDOUT: transfer009.out
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type Base1(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
        integer(k2)      j
    end type
end module

program transfer009
use m
    if(.NOT. same_type_as(transfer(Base1(20,4)(1,2), Base(20,4)(12)), &
     Base(20,4)(3))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base1(20,4)(1,2), Base(20,4)(12)), &
     Base1(20,4)(1,2))) then
        error stop 2_4
    end if

    associate(name1=>transfer(Base1(20,4)(8,9), Base(20,4)(12)))
        print *, name1
    end associate
end
