!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg516.f
! %VERIFY: fArg516.out:fArg516.vf
! %STDIN:
! %STDOUT: fArg516.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (OPTIONAL attribute allowed
!                               for passed-object dummy-arg)
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

        contains

        procedure :: print => printVal
    end type

    contains

    subroutine printVal (b)
        class (base), optional, intent(in) :: b

        if (present (b)) then
            print *, b%id
        else
            print *, 'returning'
        end if
    end subroutine
end module

program fArg516
use m
    type (base) :: b1 = base (100)

    call b1%print

    call printVal ()
end
