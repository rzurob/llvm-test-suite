! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (test the IOSTAT and IOMSG
!                               returned to parent)
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
    type base
        real(4), allocatable :: data
    end type

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (.not. allocated (dtv%data)) then
        write (unit, iomsg=iomsg) 'U'
        iostat = 500
        iomsg = 'component of dtv not allocated'
    end if
end subroutine

program fdtio504a2
use m
    integer stat
    character(200) err

    write (1, iostat=stat, iomsg=err) base(null())

    if (stat /= 500) error stop 1_4

    if (err /= 'component of dtv not allocated') error stop 2_4

    close (1, status='delete')
end