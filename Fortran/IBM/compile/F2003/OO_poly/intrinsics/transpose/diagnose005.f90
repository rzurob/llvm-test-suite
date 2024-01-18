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
! %POSTCMD: dcomp diagnose005.f
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    When MATRIX is poly, the return value of transpose shall be
!*  poly and thus shall not processed by regular IO.
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
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program diagnose005
use m
    class(Base), pointer :: b2(:,:)

    allocate(b2(4,4), SOURCE=reshape((/(Child(i,i+1),i=5,20)/),(/4,4/)))

    print *, transpose(b2)
end
