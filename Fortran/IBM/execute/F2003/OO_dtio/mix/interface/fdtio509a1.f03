! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2005
!*
!*  DESCRIPTION                : DTIO generics (test the compiler behavior of
!                               recursive IO using rewind; current behavior is
!                               SIGABRT)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), pointer :: data
    end type

    character, parameter :: ALLOC = 'A'
    character, parameter :: UNALLOC = 'U'
end module

program fdtio509a1
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

    integer stat
    character(200) msg

    write (1, iostat=stat, iomsg=msg) base(null())
end


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%data)) then
        write (unit, iostat=iostat, iomsg=iomsg)  ALLOC, dtv%data
    else
        write (unit, iostat=iostat, iomsg=iomsg)  UNALLOC
    end if

    if (iostat /= 0) return

    flush (unit)

    rewind(unit, iostat=iostat, iomsg=iomsg)    ! this is illegal operation
end subroutine
