!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/03/2005
!*
!*  DESCRIPTION                : DTIO generics (list-directed write on array
!                               constructor)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data(:)
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
end module

subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! we only do the list-directed
    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        write (unit, '(5(1x, f15.3))', iostat=iostat, iomsg=iomsg) dtv%data
    else
        write (unit, '(a)', iostat=iostat, iomsg=iomsg) 'unallocated'
    end if
end subroutine

program fdtio522
use m
    type (base), allocatable :: b1(:)
    class (base), allocatable :: b2, b3(:)

    allocate (b1(2), source=(/base((/1.7_8, 3.3_8/)), base((/3.6_8/))/))
    allocate (b2, source = base (null()))
    allocate (b3(5), source= (/(base((/i*1.1_8, i*2.2_8/)), i = 1, 5)/))

    print *, (/b1, base((/10.3_8/))/)

    print *, (/b2, b3/)

    print *, (/b2, base(null())/)
end
