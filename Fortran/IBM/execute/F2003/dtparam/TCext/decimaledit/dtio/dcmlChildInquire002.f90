! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/dtio/dcmlChildInquire002.f
! opt variations: -ql

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
!*  DATE                       : 07/10/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that INQUIRE statement in unformatted READ
!                               statement returns UNDEFINED.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: readBaseUfmtd
        generic :: read(unformatted) => readBaseUfmtd
    end type

    contains

    subroutine readBaseUfmtd (dtv, unit, iostat, iomsg)
        class(base(4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(:), allocatable :: mode

        mode = 'xlftest101'

        inquire (unit, decimal=mode)

        if (mode == 'UNDEFINED') then
            iostat = 100
            iomsg = 'inquire on decimal mode returns UNDEFINED status'
            return
        end if
    end subroutine
end module

program dcmlChildInquire002
use m
    type(base(4)) b1

    character(100) msg

    write (1) 1.23

    rewind 1

    read (1, iostat=istat, iomsg=msg) b1

    if (istat /= 100) error stop 1_4

    if (msg /= 'inquire on decimal mode returns UNDEFINED status') &
            error stop 2_4
end
