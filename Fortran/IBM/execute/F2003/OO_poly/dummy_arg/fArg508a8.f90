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
! %GROUP: fArg508a8.f
! %VERIFY: fArg508a8.out:fArg508a8.vf
! %STDIN:
! %STDOUT: fArg508a8.out
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
!*  DATE                       : 12/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for
!                               explicit-shape unlimited poly dummy arg array;
!                               associated actual-args are unlimited poly array
!                               sections)
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
        integer(4) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (*), allocatable :: x1_m(:)

    type (child), target, save :: c1_m (3:5)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine reset (b)
        class (*), intent(out) :: b(2)
    end subroutine
end module


program fArg508a8
use m
    type (base), target :: b1(3)

    class (*), pointer :: b_ptr(:)

    b1%id = (/1,2,3/)

    c1_m%id = (/3,4,5/)
    c1_m%name = (/'c1_m_3', 'c1_m_4', 'c1_m_5'/)

    b_ptr => b1

    call reset (b_ptr(::2))     !<-- b1(1), (3)

    b_ptr => c1_m(::2)

    allocate (x1_m(3), source=child(200,'x1_m'))

    call reset (b_ptr)          !<-- c1_m(3), (5)
    call reset (x1_m(2:))        !<-- x1_m(2), (3)

    do i = 1, 3
        call b1(i)%print

        call c1_m(i+2)%print

        select type (x => x1_m(i))
            class is (base)
                call x%print
            class default
                error stop 1_4
        end select
    end do

    deallocate (x1_m)
end
