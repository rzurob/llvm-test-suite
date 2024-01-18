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
! %POSTCMD: dcomp fArg014d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (constraint C544:
!                               assumed-size array with INTENT(OUT) can NOT be
!                               of a type with default initialization)
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
        integer*4 :: id = -1
    end type

    contains

    subroutine abc (b)
        class (base), intent(out) :: b(*)   !<-- illegal for base having default-init
    end subroutine
end module

program fArg014d
end
