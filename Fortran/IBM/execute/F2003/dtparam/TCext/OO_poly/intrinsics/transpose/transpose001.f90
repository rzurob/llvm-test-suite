! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/transpose/transpose001.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transpose001.f
! %VERIFY: transpose001.out:transpose001.vf
! %STDIN:
! %STDOUT: transpose001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : MATRIX is non-poly.
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
end module

program transpose001
use m
    type(Base(4)) :: b1(2,4)
    b1 = reshape((/(Base(4)(i),i=1,8)/), (/2,4/))

    print *, transpose(reshape((/(Base(4)(i),i=1,9)/), (/3,3/)))

    print *, transpose(b1)
end
