! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/transfer/transfer002.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer002.f
! %VERIFY: transfer002.out:transfer002.vf
! %STDIN:
! %STDOUT: transfer002.out
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
!*    Physical representation of result has the same length as that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD, and has the same physical representation as SOURCE.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type Base1(n2,k2)    ! (20,4)
        integer, kind     :: k2
        integer, len      :: n2
        type(Base(n2,k2)) :: b
    end type
end module

program transfer002
use m
    type(Base1(20,4)), pointer :: m1
    nullify(m1)

    if(.NOT. same_type_as(transfer(Base(20,4)(10), m1), &
     Base1(20,4)(Base(20,4)(1)))) then
        error stop 1_4
    end if

    if(same_type_as(transfer(Base(20,4)(10), m1), &
     Base(20,4)(1))) then
        error stop 2_4
    end if

    print *, transfer(Base(20,4)(10), m1)
end
