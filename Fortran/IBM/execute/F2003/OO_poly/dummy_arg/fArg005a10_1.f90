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
! %GROUP: fArg005a10_1.f
! %VERIFY: fArg005a10_1.out:fArg005a10_1.vf
! %STDIN:
! %STDOUT: fArg005a10_1.out
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
!*  DATE                       : 05/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg
!*                               together with nonpointer-poly-dummy-arg; also
!*                               put the call as the type-bound; also try named
!*                               constant or temps as actual arg)
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

        contains

        procedure :: print => printBase
        procedure, non_overridable :: copyData => copyBaseData
        final :: finalizeBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild

        final :: finalizeChild
    end type

    contains

    subroutine copyBaseData (b, b1)
        class (base), intent(in) :: b
        class (base), intent(out), pointer :: b1

        allocate (b1, source=b)
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program fArg005a10_1
use m
    type (child), parameter :: c_const = child (1, 'c_const')

    class (base), pointer :: b2, b3

    print *, 'begin'

    call c_const%copyData (b3)

    call b3%print

    call copyBaseData (child (2, 'temp'), b2)

    call b2%print

    deallocate (b2)
    deallocate (b3)

    print *, 'end'
end
