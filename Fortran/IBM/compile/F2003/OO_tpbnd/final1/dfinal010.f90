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
! %POSTCMD: dcomp dfinal010.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal010.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               Final subroutines shall not be a
!*                               function.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains

!* expect error message 1514-598 here
        final  :: finalizeBase
    end type

    contains
    integer function finalizeBase (b1)
       type(base), intent(out) :: b1
       finalizeBase = 10
    end function
end module
end
