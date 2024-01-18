! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (The effect of TL edit descriptor
!                               duing DTIO: TLn never causes the file to be
!                               positioned to before where the child data
!                               transfer started; test list-directed read)
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
        character(10), pointer :: data(:) => null()
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

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 10_4

    !! this is to test that TL will not reposition to a point before where the
    !child write started
    if (associated (dtv%data)) then
        write (unit, '(TL3, a,TL20, TL10,a)', iostat=iostat, iomsg=iomsg) dtv%data
    end if
end subroutine


program fdtio059
use m
    integer stat1
    character(200) err

    class (base), allocatable :: b1(:)

    allocate (b1(2))

    allocate (b1(1)%data(2), source=(/'0123456789', 'abcdefghij'/))

    allocate (b1(2)%data(2), source=(/'9876543210', 'ABCDEFGHIJ'/))

    open (1, file='fdtio059.data')

    write (1, *, iostat=stat1, iomsg=err) 'line1:', b1(1)
    write (1, *, iostat=stat1, iomsg=err) 'line2:', b1
end
