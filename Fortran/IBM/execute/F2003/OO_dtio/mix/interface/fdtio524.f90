!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2005
!*
!*  DESCRIPTION                : DTIO generic (DTIO generic declared inside the
!                               module becomes available to other module
!                               procedures)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data(:)

        contains

        procedure :: print => printBase
    end type

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b
    end subroutine
end module

program fdtio524
use m
    class (base), allocatable :: b1

    allocate (b1)

    allocate (b1%data(0:2), source=(/1.1_8, 2.2_8, 3.3_8/))

    call b1%print
end

!! this routine only deals with list-directed output
subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        do i = lbound(dtv%data,1), ubound(dtv%data,1)
            write (unit, '(f12.3,1x)', iostat=iostat, iomsg=iomsg) dtv%data(i)
        end do
    end if
end subroutine
