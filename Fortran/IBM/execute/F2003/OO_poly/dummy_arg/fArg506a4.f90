!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) and default
!                               initializations)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'
        type (base) :: data (3)

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

        do i = 1, 3
            call b%data(i)%print
        end do
    end subroutine
end module

program fArg506a4
use m
    class (base), allocatable :: b1 (:,:)

    type (child) :: c1 (4)

    c1%id = (/1,2,3,4/)
    c1%name = (/'c1_1','c1_2','c1_3','c1_4'/)
    c1(1)%data = (/(base (i*10), i=1,3)/)
    c1(2)%data = (/(base (i*20), i=1,3)/)
    c1(3)%data = (/(base (i*30), i=1,3)/)
    c1(4)%data = (/(base (i*40), i=1,3)/)

    allocate (b1(2,2), source=reshape(c1, (/2,2/)))

    call test1 (b1(1,:))

    call b1(2,1)%print
    call b1(2,2)%print

    contains

    subroutine test1 (b)
        class (base), intent(out) :: b (2)

        do i = 1, size (b)
            call b(i)%print
        end do
    end subroutine
end
