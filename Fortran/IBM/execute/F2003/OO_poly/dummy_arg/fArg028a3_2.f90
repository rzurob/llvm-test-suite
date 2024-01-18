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
! %GROUP: fArg028a3_2.f
! %VERIFY: fArg028a3_2.out:fArg028a3_2.vf
! %STDIN:
! %STDOUT: fArg028a3_2.out
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
!*  DATE                       : 11/25/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (sequence association for
!                               unlimited poly dummy-arg array)
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
        real(4) :: pos

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        write (*, '(f10.2)') b%pos
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print '(f10.2,a,a)', b%pos, ', name=', b%name
    end subroutine

    subroutine print2 (b)
        class (base) b(0:1,2)

        call b(0,1)%print
        call b(0,2)%print
    end subroutine

    subroutine print2X (x)
        class (*) x(2,2)

        select type (x)
            class is (base)
                call x(1,1)%print
                call x(1,2)%print
            class default
                print *, 'other types'
        end select
    end subroutine
end module

program fArg028a3_2
use m
    class (base), allocatable :: b1(:)

    type (child) :: c1 (10)

    c1%pos = (/(i*1.0, i=1,10)/)

    c1%name = 'xlftest 101'

    print *, 'test1'
    call print2X (c1(3))

    allocate (b1(0:9), source=c1)

    print *, 'test2'
    call print2X (b1)


    print *, 'test3'
    call print2X (b1(5:))   !<-- same as call print2X(c1(6))

    print *, 'test4'
    call print2X (b1(9:0:-2))
end
