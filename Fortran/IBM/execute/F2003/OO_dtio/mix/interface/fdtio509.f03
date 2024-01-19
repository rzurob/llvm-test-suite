! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (basic test on unformatted IO
!                               for stream access mode)
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
    type A
        real(4) :: d1(2)
    end type

    type base
        type (A) :: data(2)
    end type
end module

program fdtio509
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base) :: b1, b2

    logical precision_r4


    b1 = base ((/A((/1.0, 2.0/)), A ((/3.0,4.0/))/))

    open (1, access='stream')

    write (1, pos = 10) b1

    rewind(1)

    read (1, pos=10) b2

    if ((.not. precision_r4(b2%data(1)%d1(1), 12.0_4)) .or.&
        (.not. precision_r4(b2%data(1)%d1(2), 13.0_4))) error stop 1_4

    if ((.not. precision_r4(b2%data(2)%d1(1), 102.0_4)) .or.&
        (.not. precision_r4(b2%data(2)%d1(2), 103.0_4))) error stop 2_4

    close (1, status='delete')
end

subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg) dtv%data(1)

    if (iostat /= 0) return

    read (unit, iostat=iostat, iomsg=iomsg) dtv%data(2)

    if (iostat /= 0) return

    dtv%data(1)%d1 = dtv%data(1)%d1 + 10.0
    dtv%data(2)%d1 = dtv%data(2)%d1 +100.0
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg) (dtv%data(1)%d1 + 1.0)

    if (iostat /= 0) return

    write (unit, iostat=iostat, iomsg=iomsg) (dtv%data(2)%d1 - 1.0)

    if (iostat /= 0) return
end subroutine
