! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/5/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (test the repositioning of the
!                               file during the child data transfer)
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

program fdtio509a
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

    type (base) :: b1

    b1 = base ((/A((/1.0, 2.0/)), A ((/3.0,4.0/))/))

    open (1, access='stream')

    write (1, pos = 10, err=200) b1

    stop 2

200 close (1, status='delete')
end


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg, pos=100) dtv%data(1)

    if (iostat == 0) error stop 1
end subroutine
