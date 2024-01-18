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
! %GROUP: fArg026a_2.f
! %VERIFY: fArg026a_2.out:fArg026a_2.vf
! %STDIN:
! %STDOUT: fArg026a_2.out
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
!*  DATE                       : 06/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (implicit interface)
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

program fArg026a_2
use m
    type (base) :: b1 (10)
    class (base), allocatable :: b2 (:)
    type (child), target :: c1 (5)

    class (base), pointer :: b3(:)

    b1 = (/(base (i), i=10,1,-1)/)
    c1 = (/(child(i, 'c1'), i=1,5)/)

    allocate (b2 (5), source=c1)

    allocate (b3 (5))

    b3%id = (/10, 20, 30, 40, 50/)


    call increaseID (b1(2:6))

    call increaseID (b2(1:5))

    call increaseID (b3)

    do i = 1, 5
        call b1(i+1)%print
        call b2(i)%print
        call b3(i)%print
    end do

    call increaseID (b2)

    do i = 1, 5
        call b2(i)%print
    end do

    deallocate (b3, b2)
end

subroutine increaseID (b)
    use m
    type (base), intent(inout) :: b(5)

    do i = 1, 5, 2
        b(i)%id = b(i)%id + 1
    end do
end subroutine
