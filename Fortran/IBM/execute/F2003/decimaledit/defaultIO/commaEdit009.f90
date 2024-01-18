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
!*  DATE                       : 08/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use array sections in the input for namelist
!                               entity.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program commaEdit009
    real(8) d1(10)
    complex(4) cx1(6)

    logical(4), external :: precision_x8, precision_r8

    namelist /nml/ d1, cx1

    open (1, file='commaEdit009.in', decimal='COMMA')

    read (1, nml)

    !! verify results
    do i = 1, 9, 2
        if (.not. precision_r8(d1(i), 1.1d0*(i+1)/2)) error stop 1_4
        if (.not. precision_r8(d1(i+1), (1.0d0*(i+1)/2)+0.3d0)) error stop 2_4
    end do

    do i = 1, 6
        if (.not. precision_x8(cx1(i), cmplx(i*1.1d0, (i+1)*1.1d0, 4))) &
            error stop 3_4
    end do
end
