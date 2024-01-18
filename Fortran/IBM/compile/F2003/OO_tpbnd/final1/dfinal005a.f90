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
! %POSTCMD: dcomp dfinal005a.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal005a.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: A final
!*                               -subroutine-name shall not be one
!*                               previously specified as a final
!*                               subroutine for that type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final  :: finalizeBase1
!* expect error message 1514-541 here
        final  :: finalizeBase1
    end type

    contains
    subroutine finalizeBase (b1)
       type(base) :: b1
       print *, 'finalizeBase'
    end subroutine
end module
end
