!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp dfinal001b.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal001b.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: final
!*                               subroutines cannot be overidden
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final :: finalizeBase
    end type

    type, extends(base) :: child
    end type

    type, extends(child) :: thirGen
    contains

!* expect error message 1514-596 here
       final :: finalizeBase
    end type

    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'finalizeBase'
    end subroutine
end module
end
