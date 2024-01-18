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
!*  DATE                       : 06/22/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 321923)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: c2, c3

    allocate(c2, source = 'IBM')
    allocate(c3, source = ' XLF')

    associate (x => c(c2))

        allocate (c2, source=c3//x)
    end associate

    if (c2 /= ' XLFIBM') error stop 1_4

    contains

    function c(c1)
        character(:), allocatable :: c

        character(:), allocatable :: c1

        call move_alloc(c1, c)
    end function
    end
