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
! %GROUP: fArg031a1.f
! %VERIFY: fArg031a1.out:fArg031a1.vf
! %STDIN:
! %STDOUT: fArg031a1.out
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
!                               arg; use deferred-shape arrays)
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
        integer*4 :: id = 1

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
        class (base), intent(inout), allocatable :: b(:)

        call test2 (b)
        call test3 (b)
    end subroutine

    subroutine test2 (b)
        class (base), intent(inout), allocatable :: b(:)

        if (.not. allocated (b)) then
            allocate (b(10), source=child(1,'default'))

            call assignID (b)
        else
            call resetID (b(::2))
        end if
    end subroutine

    subroutine assignID (b)
        type (base), intent(inout) :: b(:)

        b%id = (/(i,i=1,size(b))/)
    end subroutine

    subroutine resetID (b)
        type (base), intent(out) :: b(:)
    end subroutine

    subroutine test3 (b)
        class (base), intent(inout), allocatable :: b(:)

        b(2::2)%id = b(2::2)%id * 2
    end subroutine
end module

program fArg031a1
use m
    class (base), allocatable :: b1 (:)

    type (child) :: c1 (10)

    c1%id = (/(i,i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i-1), i=1,10)/)

    call test1 (b1)

    do i = 1, 10
        call b1(i)%print
    end do

    deallocate (b1)

    allocate (b1(10), source=c1)

    call test1 (b1)

    do i = 1, 10
        call b1(i)%print
    end do
end
