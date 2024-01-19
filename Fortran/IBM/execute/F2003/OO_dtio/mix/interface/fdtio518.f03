! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (multiple derived type components
!                               handled by DTIO)
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
    type base1
        real, pointer :: data(:)
    end type

    type base2
        complex, allocatable :: data(:)
    end type


    interface write(formatted)
        subroutine formattedWriteBase1 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base1
            class (base1), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedWriteBase2 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base2
            class (base2), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


!! only deal with list-directed write
subroutine formattedWriteBase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1
    class (base1), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 11_4

    if (associated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
    end if
end subroutine

subroutine formattedWriteBase2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base2
    class (base2), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 21_4

    if (allocated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
    end if
end subroutine


module n
use m
    type dataType
        type (base1) :: b1
        type (base2) :: b2
    end type
end module


program fdtio518
use m
use n
    type (dataType) :: d1(2)

    allocate (d1(1)%b1%data(2), source=(/1.0, 2.0/))
    allocate (d1(1)%b2%data(-1:0), source= (/(3.0, 2.0), (4.0, 2.0)/))

    nullify(d1(2)%b1%data)

    allocate (d1(2)%b2%data(3), source=(/(1.3, 2.5), (2.3, 465.), (12.,1.2)/))

    !! test the write for scalar
    print *, d1(1)

    !! test the write for array
    print *, d1
end
