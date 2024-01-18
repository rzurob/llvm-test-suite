!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2005
!*
!*  DESCRIPTION                : DTIO generics (null value effects on
!                               list-directed DTIO read)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data
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
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=-1.0)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio515
use m
    class (base), allocatable :: b1(:,:)

    allocate (b1(2,2))

    allocate (b1(1,1)%data, b1(1,2)%data, b1(2,2)%data)

    open (10, file= 'fdtio515.data')

    write (10, *) '1.2, 2.1, 3*, 4.5, 2.3'
    write (10, *) '2.2, /'
    write (10, *) ' 5.5, 2.3'

    rewind (10)

    read (10, *) b1

    write (*, '(4(f10.2))') b1(1,1)%data, b1(2,1)%data, b1(1,2)%data, b1(2,2)%data

    read (10, *) b1

    write (*, '(4(f10.2))') ((b1(i,j)%data, i=1,2), j=1,2)

    close (10, status='delete')
end
