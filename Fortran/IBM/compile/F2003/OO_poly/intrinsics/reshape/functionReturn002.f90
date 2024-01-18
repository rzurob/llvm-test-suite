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
!*  DATE                       : 11/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of an
!*                               external function call. Diagnostic
!*                               test case. When return value of
!*                               reshape is poly, it shall not be
!*                               processed by IO.
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

    contains

    function func1()
        class(Base), allocatable :: func1(:)
        allocate(func1(20), SOURCE=(/ (Child(i+1,i-1), i=1,20) /))
    end function
end module

program functionReturn002
use m
    print *, reshape(func1(), (/3,5/), (/Child(22,33)/), (/1,2/))
end
