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
! %GROUP: fArg004a2.f
! %VERIFY: fArg004a2.out:fArg004a2.vf
! %STDIN:
! %STDOUT: fArg004a2.out
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
!*  DATE                       : 06/07/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly-dummy-arg-array of
!                               explicit-shape associated with poly-allocatable
!                               of different bounds setting; also the dummy-arg
!                               is used as actual-arg to be associated with
!                               non-poly dummy-arg-array)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg004a2
use m
    type (child) :: c1 (4)
    class (base), allocatable :: b1(:)

    c1 = (/(child (i, 'c1'),i=1,4)/)

    allocate (b1(2:5), source=c1)

    call test1 (b1)

    call test2 (b1)

    contains

    subroutine test1 (b)
        class (base), intent(in) :: b(4)

        call b(1)%print
        call b(2)%print
        call b(3)%print
        call b(4)%print

        call test2 (b)
    end subroutine

    subroutine test2 (b)
        type(base), intent(in) :: b(4)

        call b(1)%print
        call b(2)%print
        call b(3)%print
        call b(4)%print
    end subroutine
end
