
subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character (20) :: temp

    if (.not. allocated (dtv%i1)) allocate (dtv%i1)

    read (unit, *, iostat=iostat, iomsg=iomsg) temp, dtv%i1
end subroutine

subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%i1)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) 'id= ', dtv%i1
    end if
end subroutine
