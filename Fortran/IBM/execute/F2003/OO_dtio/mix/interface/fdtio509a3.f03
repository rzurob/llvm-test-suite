! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2005
!*
!*  DESCRIPTION                : DTIO generics (test inquire statement during
!                               DTIO)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4), pointer :: i
    end type

    integer(4), parameter :: ISNULL = -999999
end module

module m1
use m
    interface read(unformatted)
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio509a3
use m
use m1, only: read (unformatted)
    type (base) :: b1

    character(8) :: pos

    integer stat1
    character(200) err

    write(1) 100

    inquire(1, position=pos)

    print *, pos

    rewind 1

    inquire(1, position=pos)

    print *, pos

    read (1, iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    inquire(1, position=pos)

    print *, pos

    close(1, status='delete')
end


subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(20) :: pos

    read (unit, iostat=iostat, iomsg=iomsg) i1

    if (iostat /= 0) return

    inquire (unit, position=pos)

    if (pos /= 'ASIS') then
        iostat = 1000
        iomsg = 'inquire failed'

        return
    end if

end subroutine
