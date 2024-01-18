! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/F2003/decimaledit/dtio/d323050a.f
! opt variations: -qnock -qnol

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

    type A(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        character(:), allocatable :: name
        real(k1), allocatable :: data
    end type

    type (A(20,4)) a1(10), a2

    logical(4), external :: precision_r4

    !! test 1: unallocated component will be allocated automatically
    a2%name = 'xlf'
    a2%data = -1.0

    do i = 1, 10
        a1(i)%name = 'xlftest'
        a1(i)%data = i
    end do

    if ((a2%name /= 'xlf') .or. (a2%name%len /= 3)) error stop 1_4

    if (.not. precision_r4(-1.0, a2%data)) error stop 3_4

    do i = 1, 10
        if ((a1(i)%name /= 'xlftest') .or. (a1(i)%name%len /= 7)) &
                error stop 2_4

        if (.not. precision_r4 (a1(i)%data, i*1.0)) error stop 4_4
    end do

    !! test 2: allocated component will be reallocated by the fact that the
    !deferred length mis-match between var. and expr.
    a2%name = 'xlftest'
    a2%data = -1.4

    do i = 1, 10
        a1(i)%name = 'xlftest '//achar(iachar('0') + i-1)

        a1(i)%data = a1(i)%data + 1
    end do

    if ((a2%name /= 'xlftest') .or. (len(a2%name) /= 7)) error stop 5_4

    if (.not. precision_r4(a2%data, -1.4_4)) error stop 7_4

    do i = 1, 10
        if ((len(a1(i)%name) /= 9) .or. (a1(i)%name /= &
                'xlftest '//achar(iachar('0') + i-1))) error stop 6_4


        if (.not. precision_r4(a1(i)%data, (i+1)*1.0_4)) error stop 8_4
    end do
    end
