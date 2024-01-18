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
! %POSTCMD: dcomp fArg005d6.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (for pointer dummy-arg
!*                               the declared type of the actual-arg shall be
!*                               the same as that of the dummy-arg)
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
        integer*4 :: id
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    contains

    subroutine abc (b1)
        class (base), pointer :: b1
    end subroutine

    subroutine cba (b2)
        class (base), pointer :: b2(:)
    end subroutine
end module

program fArg005d6
use m
    class (child), pointer :: c1
    class (child), pointer :: c2(:)

    type (child), pointer :: c3

    call abc (c1)

    call cba (c2)

    call abc (c3)
end
