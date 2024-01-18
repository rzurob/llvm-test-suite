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
! %GROUP: fArg031a.f
! %VERIFY: fArg031a.out:fArg031a.vf
! %STDIN:
! %STDOUT: fArg031a.out
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
!*  DATE                       : 06/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (dummy-arg used as actual
!                               arg; use assumed-shape arrays)
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

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        type (base), intent(inout) :: b(:)

        call test2 (b(::2))
        call test3 (b(2::2))
    end subroutine

    subroutine test2 (b)
        class (base), intent(inout) :: b(:)

        b(::2)%id = b(::2)%id*2
    end subroutine

    subroutine test3 (b)
        class (base), intent(inout) :: b(3:)

        b%id = b%id + 1
    end subroutine
end module

program fArg031a
use m
    class (base), allocatable :: b1 (:)

    type (child) :: c1 (10)

    c1%id = (/(i,i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i-1), i=1,10)/)

    allocate (b1(10), source=c1)

    call test1 (b1)

    do i = 1, 10
        call b1(i)%print
    end do
end
