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
!                               Case: Mis-use of assumed type-param in allocate
!                               statement; for unlimited poly type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtSpec009d
    type A (n)
        integer, len :: n = 100

        integer ids(n)
    end type

    class(*), allocatable :: x

    contains

    subroutine testA (x)
        class(*), allocatable :: x

        allocate (A(*) :: x)     !<-- illegal
        allocate (A(:) :: x)     !<-- illegal
    end subroutine
end
