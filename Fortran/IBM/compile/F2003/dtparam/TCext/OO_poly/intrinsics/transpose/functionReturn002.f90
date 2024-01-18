! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/intrinsics/transpose/functionReturn002.f
! opt variations: -qnol -qnodeferredlp

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
! %POSTCMD: dcomp functionReturn002.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case. When return value of transpose is poly,
!*  it shall not be processed by regular IO.
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

program functionReturn002
use m
    class(Base(:,4)), allocatable :: b1(:,:)
    allocate(b1(2,4), SOURCE=reshape((/(Base(20,4)(i),i=1,8)/), (/2,4/)))

    print *, transpose(b1)
end
