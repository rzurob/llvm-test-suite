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
! %GROUP: ftpbnd508a1.f
! %VERIFY: ftpbnd508a1.out:ftpbnd508a1.vf
! %STDIN:
! %STDOUT: ftpbnd508a1.out
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
!*  DATE                       : 03/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (one procedure bound to
!*                               multiple bindings)
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

        procedure, NOPASS :: print => printBase
        procedure :: printSelf => printBase
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure, nopass :: printBase
    end type
end module

program ftpbnd508a1
use m1, only : child, base

    type, extends (child) :: thirdGeneration
    end type

    type (base) :: b1

    type (child) :: c1

    type (thirdGeneration) :: t1

    b1%id = 10

    c1%id = 100
    c1%name = 'c1'

    t1%id = 1000
    t1%name = 't1'

    call b1%print (c1)
    call b1%print (b1)
    call b1%print (t1)

    call c1%printBase(c1)
    call c1%printBase(b1)
    call c1%printBase(t1)

    call t1%printBase(c1)
    call t1%printBase(b1)
    call t1%printBase(t1)

    call c1%print (c1)
    call c1%print (b1)
    call c1%print (t1)

    call t1%print (c1)
    call t1%print (b1)
    call t1%print (t1)

    call c1%printSelf
    call b1%printSelf
    call t1%printSelf
end
