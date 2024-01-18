!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg025a4.f
! %VERIFY: 
! %STDIN:
! %STDOUT: fArg025a4.out
! %EXECARGS:
! %POSTCMD: spiff -r.000001 fArg025a4.out $TR_SRC/fArg025a4.vf && rm -f fArg025a4.out
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (intrinsic elemental
!                               procedure can be used as actual argument)
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

program fArg025a4

    intrinsic abs   !<-- this is an elemental func

    call test1 (abs, (/-1.0, -2.0, -.5/))

    contains

    subroutine test1 (func, r)
        procedure (real*4) func
        real*4, intent(in) :: r (:)

        do i = 1, size(r)
            print *, func(r(i))
        end do
    end subroutine
end
