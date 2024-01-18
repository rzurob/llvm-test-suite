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
! %GROUP: fArg010a3.f
! %VERIFY: fArg010a3.out:fArg010a3.vf
! %STDIN:
! %STDOUT: fArg010a3.out
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
!*  DATE                       : 06/29/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (VALUE attribute; test
!                               non-pointer component)
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

        procedure :: assignID => assignID2Base
        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    !! this subroutine will be reseting the value of other component in the
    !extended types
    subroutine assignID2Base (b, id)
        class (base), intent(out) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fArg010a3
use m
    type (child) :: c1 = child (1, 'c1')

    if (resetTemp (c1) /= 1) error stop 1_4

    call c1%print

    contains

    integer(4) function resetTemp (b)
        class (base) :: b

        resetTemp = b%id

        select type (b)
            class is (child)
                call resetIDChild (b)

            class default
                call resetIDBase (b)

        end select
    end function

    subroutine resetIDBase (b)
        type(base), value :: b

        call b%assignID (100)

        if (b%id /= 100) error stop 10_4

        call b%print
    end subroutine

    subroutine resetIDChild (b)
        type(child), value :: b

        call b%assignID (100)

        if (b%id /= 100) error stop 20_4

        call b%print
    end subroutine
end
