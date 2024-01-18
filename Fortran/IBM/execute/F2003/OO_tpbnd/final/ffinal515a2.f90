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
! %GROUP: ffinal515a2.f
! %VERIFY: ffinal515a2.out:ffinal515a2.vf
! %STDIN:
! %STDOUT: ffinal515a2.out
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
!*  DATE                       : 04/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               structure constructor in CASE construct)
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

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    interface operator (==)
        logical function baseEqual (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
        end function

        logical function childEqual (c1, c2)
        import child
            type (child), intent(in) :: c1, c2
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

logical function baseEqual (b1, b2)
use m, only: base
    type (base), intent(in) :: b1, b2

    baseEqual = (b1%id == b2%id)
end function

logical function childEqual (c1, c2)
use m, only: child, operator(==), base
    type (child), intent(in) :: c1, c2

    childEqual = ((c1%base == c2%base) .and. (c1%name == c2%name))
end function

program ffinal515a2
use m
    type (child) :: c1 = child (10, 'c1_static')
    type (base) :: b1 = base (100)

    select case (c1 == child (10, 'temp'))
        case (.true.)
            error stop 1_4
        case (.false.)
            print *, 'success'
    end select

    select case (b1 == base (100))
        case (.true.)
            print *, 'success'
        case (.false.)
            error stop 2_4
    end select

    print *, 'end'
end
