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
! %POSTCMD: tcomp fArg014d1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/06/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-size array used
!                               as the selector; associate-name must be
!                               assumed-size array then)
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
        integer id
    end type

    type, extends(base) :: child
        character(15) name
    end type
end module

program fArg014d1
use m

    call abc ((/base(10), base(20), base(30)/))

    contains

    subroutine abc (b)
        class (base), intent(in) :: b(*)

        select type (b)
            type is (base)
                print *, size(b)        !<-- illegal query
        end select
    end subroutine
    end
