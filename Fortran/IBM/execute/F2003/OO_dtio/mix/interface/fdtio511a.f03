! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (zero-size arrays on effective
!                               items)
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
        integer id(2)
    end type

    type B
        character(20) :: name
    end type

    type base
        type (A) a1
        type (B) b1
    end type
end module


program fdtio511a
use m
    interface read(formatted)
        subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (A), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedReadB (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (B), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(formatted)
        subroutine formattedWriteA (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (A), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedWriteB (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (B), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base) b1

    b1%a1 = A((/10, 20/))

    print *, base(a((/1,2/)), b('xlftest'))

    write (1, *) base(a((/1,2/)), b('xlftest team'))

    rewind(1)

    read (1, *) b1

    if (any (b1%a1%id /= (/10, 20/))) error stop 1_4

    if (b1%b1%name /= 'xlftest team') error stop 2_4

    close (1, status='delete')
end


subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (A), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, '(i6)', iostat=iostat, iomsg=iomsg) dtv%id(2:1)
end subroutine


subroutine formattedReadB (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (B), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, '(a20)', iostat=iostat, iomsg=iomsg) dtv%name
end subroutine

subroutine formattedWriteA (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (A), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(i5)', iostat=iostat, iomsg=iomsg) dtv%id(2:1)
end subroutine


subroutine formattedWriteB (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (B), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(a20)', iostat=iostat, iomsg=iomsg)  dtv%name
end subroutine
