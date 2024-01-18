!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg503.f
! %VERIFY: fArg503.out:fArg503.vf
! %STDIN:
! %STDOUT: fArg503.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : arg-association (value attribute for dummy-arg)
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

    contains

    subroutine printBase (b)
        type (base), value :: b

        call printBaseNoVal (b)
    end subroutine

    subroutine printBaseNoVal (b)
        class (base) :: b

        print *, b%id
    end subroutine

end module

program fArg503
use m
    type (base) b1

    b1 = base (10)

    call printBase(b1)
end
