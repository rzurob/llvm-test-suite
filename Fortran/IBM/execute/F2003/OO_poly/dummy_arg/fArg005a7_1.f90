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
! %GROUP: fArg005a7_1.f
! %VERIFY: fArg005a7_1.out:fArg005a7_1.vf
! %STDIN:
! %STDOUT: fArg005a7_1.out
! %EXECARGS:
! %POSTCMD: 
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
!*  DATE                       : 06/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited
!                               poly-allocatable dummy-arg to be only associated
!                               with the unlimited poly-allocatable actual-arg;
!                               use of the derived type to test the vft for the
!                               finalizer)
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
        integer(4) id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    subroutine createData (x, x1)
        class (*), allocatable, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg005a7_1
use m
    type (child) :: c1

    class (*), allocatable :: x

    allocate (base :: x)

    print *, 'calling createData'

    call createData (x, c1)

    print *, 'calling deallocate'

    deallocate (x)
end
