!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio053a.f
! %VERIFY: fdtio053a.data:fdtio053a.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fdtio053a.data
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (try inquire the file name during
!                               DTIO write formatted)
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
end module

program fdtio053a
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

    open (10, file='fdtio053a.data')

    write (10, *) p1

    write (10, *)  point(2.23, y=1.75)

end

subroutine writePointFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (point), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(100) localName

    if (unit > 0) then
        inquire (unit, name=localName, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return
    end if

    write (unit, *) 'fileName: ', localName

    write (unit, 100, iostat=iostat, iomsg=iomsg) dtv%x, dtv%y
100 format ("x=", f10.2, ",  y=",f10.2)
end subroutine
