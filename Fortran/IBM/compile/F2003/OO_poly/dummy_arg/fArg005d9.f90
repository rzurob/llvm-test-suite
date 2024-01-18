!#######################################################################
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
! %POSTCMD: dcomp fArg005d9.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (if the dummy-arg is a
!*                               pointer, the actual arg shall be a pointer)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    end type
end module

program fArg005d9
use m
    class (base), allocatable :: b1
    class (*), allocatable :: x1

    call abc (b1)

    call cba (x1)

    contains

    subroutine abc (b)
        class (base), pointer :: b
    end subroutine

    subroutine cba (x)
        class (*), pointer :: x
    end subroutine
end
