! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/spread/diagnose009.f
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
! %POSTCMD: dcomp diagnose009.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    The return value of spread is polymorphic and shall not be
!*  handled by regular IO.
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
        integer(k1)   :: i = 9
    end type
end module

program diagnose009
use m
    class(*), pointer :: b1(:)
    allocate(b1(3), SOURCE=(/Base(20,4)(1),Base(20,4)(2),Base(20,4)(3)/))
    print *, spread(b1, 1, 2)
end
