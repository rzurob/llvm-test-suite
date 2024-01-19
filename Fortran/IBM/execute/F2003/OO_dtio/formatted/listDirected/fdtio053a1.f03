! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (try inquire statement for file
!                               named on the unit during DTIO)
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
    type point
        real*4 :: x, y
    end type

    logical isNamedFile
end module

program fdtio053a1
use m
    interface WRITE(FORMATTED)
        subroutine writePointFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (point), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (point) :: p1

    p1 = point (1.34, 3.21)

    print *, p1

    write (*, *)  point(2.23, y=1.75)
end

subroutine writePointFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (point), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (unit > 0) then
        inquire (unit, named=isNamedFile, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return
    end if

    write (unit, '(L1, 2f10.2)', iostat=iostat, iomsg=iomsg) isNamedFile, dtv%x, dtv%y
end subroutine
