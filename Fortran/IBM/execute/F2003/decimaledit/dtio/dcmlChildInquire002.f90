!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/10/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that INQUIRE statement in unformatted READ
!                               statement returns UNDEFINED.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        procedure :: readBaseUfmtd
        generic :: read(unformatted) => readBaseUfmtd
    end type

    contains

    subroutine readBaseUfmtd (dtv, unit, iostat, iomsg)
        class(base), intent(inout) :: dtv
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
    type(base) b1

    character(100) msg

    write (1) 1.23

    rewind 1

    read (1, iostat=istat, iomsg=msg) b1

    if (istat /= 100) error stop 1_4

    if (msg /= 'inquire on decimal mode returns UNDEFINED status') &
            error stop 2_4
end
