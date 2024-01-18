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
! %GROUP: fpAssgn009a4.f
! %VERIFY: fpAssgn009a4.out:fpAssgn009a4.vf
! %STDIN:
! %STDOUT: fpAssgn009a4.out
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
!*  DATE                       : 07/07/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (derived type with BIND
!                               attribute in pointer assignment)
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
use iso_c_binding

    type, bind(c) :: bType
        integer(c_short) :: i1
        integer(c_int) :: i2
    end type
end module

module m1
use m
    contains

    subroutine printSeq (x)
        class (*), target, intent(in) :: x(:)

        type (bType), pointer :: s1 (:)

        s1 => x

        do i = 1, size (s1)
            print *, s1(i)
        end do
    end subroutine

    subroutine assignVal (x)
        class (*), target, allocatable, intent(inout) :: x (:)

        type (bType), pointer :: b1 (:)

        if (.not. allocated (x)) return

        b1 => x

        do i = lbound(b1,1), ubound(b1, 1)
            b1(i) = bType (i, 10*i)
        end do
    end subroutine
end module

program fpAssgn009a4
use m1
    type (bType), target :: b1 (10)
    class (*), allocatable, target :: x (:)
    class (*), pointer :: x1 (:)

    type (bType), pointer :: b_ptr (:)

    b1 = (/(bType(i1=i,i2 = i*10), i= 1, 10)/)

    x1 => b1 (::3)

    call printSeq (x1)

    allocate (bType :: x (2:11))

    call assignVal (x)

    b_ptr => x (::3)

    if (size (b_ptr) /= 4) error stop 1_4

    print *, 'second print'

    call printSeq (b_ptr)

    print *, 'last print'

    call printSeq (x (3::5))
end
