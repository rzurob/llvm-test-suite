! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/08/2005
!*
!*  DESCRIPTION                : DTIO generics (IO on internal file during DTIO
!                               on the same internal unit)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class (*), pointer :: data(:) => null()
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    character(50) :: buffer
end module

!! we read in 3 integer(4)
subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, buffer
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') error stop 10_4

    if (size(v_list) /= 0) error stop 11_4

    allocate (integer :: dtv%data(3))

    select type (x => dtv%data)
        type is (integer)

            !! read from unit given to the subroutine
            read (unit, *, iostat=iostat, iomsg=iomsg) x

            if (iostat /= 0) return

            !! write to buffer that data in reverse order
            !! NOTE: not clear if this is a legal operation at this time
            write (buffer, '(3i3)', iostat=iostat, iomsg=iomsg) x(3:1:-1)

        class default
            error stop 12_4
    end select
end subroutine


program fdtio054a1
use m
    class (base), allocatable :: b1(:)

    integer stat1
    character (200) err

    write (buffer, *) (i, i=1,10)

    allocate (b1(2))

    read (buffer, *, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    if (buffer /= '  3  2  1') error stop 3_4

    select type (x => b1(1)%data)
        type is (integer)
            if ( any( x /= (/1, 2, 3/))) error stop 4_4
        class default
            error stop 5_4
    end select
end
