! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/transpose/diagnose004.f
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
! %POSTCMD: dcomp diagnose004.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    MATRIX is array but not rank two. Poly and unlimited poly.
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
end module

program diagnose004
use m
    class(Base(4)), pointer :: b1(:,:,:)
    class(*), allocatable :: b2(:,:)

    allocate(b1(2,2,2), SOURCE=reshape((/(Base(4)(i),i=1,8)/), (/2,2,2/)))
    allocate(b2(2,4), SOURCE=reshape((/(Base(4)(i),i=1,8)/), (/2,4/)))

    print *, transpose(b1)
    print *, transpose(reshape(b2,(/2,2,2/)))
end
