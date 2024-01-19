! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/18/2005
!*
!*  DESCRIPTION                : DTIO generics (DTIO and associate construct)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, private, allocatable :: r1(:)
    end type

    interface write(formatted)
        module procedure formattedWrite
    end interface

    interface base
        module procedure createBaseObj
    end interface

    contains

    !! only deal with list-directed write
    subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        if (allocated (dtv%r1)) then
            write (unit, '(/,8f10.2)', iostat=iostat, iomsg=iomsg) dtv%r1
        end if
    end subroutine

    type (base) function createBaseObj (r1)
        real, intent(in) :: r1(:)

        allocate (createBaseObj%r1(size(r1)), source = r1)
    end function
end module

program fdtio523
use m
    associate (x => (/base((/1.3, 4.2, 3.1/)), base((/3.3, 4.4, 6.1, 12.3/))/))
        print *, x
    end associate
end
