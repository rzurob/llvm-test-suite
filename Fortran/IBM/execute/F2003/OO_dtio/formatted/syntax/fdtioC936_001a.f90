! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/5/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (DTIO on sequence type, type
!                               definiton is over multiple scoping units)
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
        sequence

        integer(4) :: id
        character(200) :: name
    end type
end module

program fdtioC936_001
    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            type (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            type (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type base
        sequence

        integer(4) :: id
        character(200) :: name
    end type

    type (base) b1

    b1 = base(100, 'xlftest 101')

    write (1, *) b1

    rewind (1)

    read (1, *) b1

    if (b1%id /= 111) error stop 1_4
    if (b1%name /= 'xlftest 101') error stop 2_4

    close(1, status='delete')
end

subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    type (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! use i6 for dtv%id to account for the leading blank character in list
    !directed write
    read (unit, '(i3, a20)', iostat = iostat, iomsg = iomsg) dtv%id, dtv%name

    dtv%id = dtv%id + 10
end subroutine

subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    type (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(i5, a20)', iostat = iostat, iomsg = iomsg) dtv%id+1, dtv%name
end subroutine
