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
! %POSTCMD: dcomp diagnose003.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/04/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    DIM shall be scalar and of type integer with value in the range
!*  of 1<=DIM<=n+1, where n is th rank of SOURCE.
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
        integer :: i = -1
    end type
end module

program diagnose003
use m
    type(Base) :: b1(2,2)
    print *, spread(b1, 0, 2)
end
