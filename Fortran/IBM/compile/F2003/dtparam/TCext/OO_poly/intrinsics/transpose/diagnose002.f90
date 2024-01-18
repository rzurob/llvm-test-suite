! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/transpose/diagnose002.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp diagnose002.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    MATRIX is array but not rank two. Non-poly.
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
end module

program diagnose002
use m
    type(Base(20,4)) :: b1(2,2,2)
    b1 = reshape((/(Base(20,4)(i),i=1,8)/), (/2,2,2/))

    print *, transpose((/Base(20,4)(1),Base(20,4)(2)/))
    print *, transpose(b1)
end
