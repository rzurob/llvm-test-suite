! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio053akl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio053a by Jim Xia)
!*  DATE                       : 2007-06-19 (original: 11/30/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
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
    type point (kp)
       integer, kind :: kp
        real(kp) :: x, y
    end type
end module

program fdtio053akl
use m
    interface WRITE(FORMATTED)
        subroutine writePointFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (point(4)), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (point(4)) :: p1

    p1 = point(4) (1.34, 3.21)

    open (10, file='fdtio053akl.data')

    write (10, *) p1

    write (10, *)  point(4)(2.23, y=1.75)

end

subroutine writePointFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (point(4)), intent(in) :: dtv
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
