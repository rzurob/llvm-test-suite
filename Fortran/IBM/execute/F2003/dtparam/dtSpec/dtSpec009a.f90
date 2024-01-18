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
!*  DATE                       : 02/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: use of derived-type-spec in the internal
!                               procedure.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtSpec009a
    type A (n)
        integer, len :: n = 100

        integer ids(n)
    end type

    class(*), allocatable :: x

    call testA (x)

    select type (x)
        type is (A(*))
        class default
            stop 10
    end select

    contains

    subroutine testA (x)
        class(*), allocatable :: x

        allocate (A(10) :: x)
    end subroutine
end
