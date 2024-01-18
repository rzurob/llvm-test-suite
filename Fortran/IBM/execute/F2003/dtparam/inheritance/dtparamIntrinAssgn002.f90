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
!*  DATE                       : 12/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: intrinsic assignment on derived type with
!                               parameterized components which are also
!                               polymorphic.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type arrayTemplate (k, n)
        integer, kind :: k = 4
        integer, len :: n

        contains

        procedure :: printAT
        procedure :: printAT8
        generic :: print => printAT, printAT8
    end type

    type container (k, n)
        integer, kind :: k
        integer, len  :: n

        class (arrayTemplate(k, n)), allocatable :: data
    end type

    contains

    subroutine printAT (at)
        class (arrayTemplate(n=*)), intent(in) :: at
    end subroutine

    subroutine printAT8 (at)
        class (arrayTemplate(8, n=*)), intent(in) :: at
    end subroutine
end module

module m1
use m, only: arrayTemplate
    type, extends(arrayTemplate) :: realArray
        real(k) value (n)

        contains

        procedure :: printAT
        procedure :: printAT8
    end type

    contains

    subroutine printAT (at)
        class (realArray(4, *)), intent(in) :: at

        write (*, *) 'realArray of kind 4'
        write (*, 100) at%value
100     format (8g12.2)
    end subroutine

    subroutine printAT8 (at)
        class (realArray(8, *)), intent(in) :: at

        write (*, *) 'realArray of kind 8'
        write (*, 100) at%value
100     format (8g15.5)
    end subroutine
end module

program dtparamIntrinAssgn001
use m
use m1
    type (container(4, 10)) c1, c2
    type (container(8, 20)) c3(3), c4

    allocate (c1%data, source=realArray(4, 10)(value=(/(i*1.0e1, i=1,10)/)))

    allocate (c4%data, source=realArray(8, 20)((/(i*1.0d0, i=1, 20)/)))

    !! test scalar assignment
    c2 = c1

    if (.not. allocated(c2%data))   error stop 1_4

    call c2%data%print

    !! test array assignment
    c3(1:3:2) = c4

    if (allocated(c3(2)%data))   error stop 2_4

    call c3(1)%data%print
    call c3(3)%data%print
end
