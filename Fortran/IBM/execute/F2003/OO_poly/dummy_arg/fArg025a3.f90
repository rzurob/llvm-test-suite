!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg025a3.f
! %VERIFY: fArg025a3.out:fArg025a3.vf
! %STDIN:
! %STDOUT: fArg025a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (pure actual-arg can be
!                               associated with dummy procedure)
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
    interface
        pure integer*4 function funcPure (i)
            integer*4, intent(in) :: i
        end function

        integer*4 function nonPureFunc (i)
            integer*4, intent(in) :: i
        end function
    end interface

    contains

    subroutine test1 (func, i)
        procedure(nonPureFunc) :: func
        integer*4, intent(in) :: i

        print *, func(i)
    end subroutine
end module

program fArg025a3
use m
    integer*4 :: i1 = -1

    procedure (funcPure) :: func1

    call test1 (func1, 10)

    call test1 (i=i1,func=func1)
end

pure integer*4 function func1 (i)
    integer*4, intent(in) :: i

    func1 = i + 100
end function
