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
! %GROUP: ffinal514a9.f
! %VERIFY: ffinal514a9.out:ffinal514a9.vf
! %STDIN:
! %STDOUT: ffinal514a9.out
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
!*  DATE                       : 04/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (in print statement, function return
!*                               results may create temp; finalize them after
!*                               the statement)
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

    private finalizeBase

    contains

    subroutine finalizeBase (b)
        type (base), intent (in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child), intent (in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514a9
use m1

    interface produceObj
        function produceBase (i)
        use m
            type (base) produceBase
            integer*4, intent(in) :: i
        end function

        function produceChildObj (i, c)
        use m1
            type (child) produceChildObj
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    print *, produceObj (10)
    print *, produceObj (10, 'test1')
    print *, 'end'
end

function produceBase (i)
use m
    type (base) produceBase
    integer*4, intent(in) :: i

    produceBase%id = i
end function

function produceChildObj (i, c)
use m1, only : child
    type (child) produceChildObj
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    produceChildObj%id = i
    produceChildObj%name = c
end function
