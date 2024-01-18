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
!*  DATE                       : 07/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               miscellaneous (defect 323050)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A
        character(:), allocatable :: name
    end type

    type (A) a1(10), a2

    !! test 1: unallocated component will be allocated automatically
    a2%name = 'xlf'

    do i = 1, 10
        a1(i)%name = 'xlftest'
    end do

    if ((a2%name /= 'xlf') .or. (a2%name%len /= 3)) error stop 1_4

    do i = 1, 10
        if ((a1(i)%name /= 'xlftest') .or. (a1(i)%name%len /= 7)) &
                error stop 2_4
    end do

    !! test 2: allocated component will be reallocated by the fact that the
    !deferred length mis-match between var. and expr.
    a2%name = 'xlftest'

    do i = 1, 10
        a1(i)%name = 'xlftest '//achar(iachar('0') + i-1)
    end do

    if ((a2%name /= 'xlftest') .or. (len(a2%name) /= 7)) error stop 5_4

    do i = 1, 10
        if ((len(a1(i)%name) /= 9) .or. (a1(i)%name /= &
                'xlftest '//achar(iachar('0') + i-1))) error stop 6_4
    end do
    end
