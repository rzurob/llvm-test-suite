!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio507a7_1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (a test on error condition
!                               that caused by writing more data than specified in
!                               recl=; verify iostat and iomsg)
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
        real(4), dimension(0:2) :: d1 = (/1.0, 2.0, 3.0/)
    end type
end module

program fdtio507a7
use m
    call writeData
end

subroutine writeData
use m
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    class (base), allocatable :: b1

    integer stat
    character(8) err

    err = 'no error'

    allocate (b1)

    open (1, file='fdtio507a7.data', access='direct', form='unformatted', &
            recl=20)

    write (1, iostat=stat, iomsg=err, rec=2) b1

    !! the following values are of XLF specific
    if (stat /= 3) error stop 1_4

    if (err /= '1525-003') error stop 2_4

    close (1, status='delete')
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! the 1st write will eat up 15 spaces
    write (unit, iostat=iostat, iomsg=iomsg) 'abcdefghijklmn '

    if (iostat /= 0) return

    !! there is no more spaces for this write
    write (unit, iostat=iostat, iomsg=iomsg) (dtv%d1+1)
end subroutine

