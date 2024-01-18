! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/spread/diagnose001.f
! opt variations: -ql

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
! %POSTCMD: dcomp diagnose001.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/04/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    The rank of SOURCE shall be less than 20.
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
        integer(k1)   :: i = 9
    end type
end module

program diagnose001
use m
    type(Base(4)) :: b1(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
    print *, size(spread(b1, 1, 2))
end
