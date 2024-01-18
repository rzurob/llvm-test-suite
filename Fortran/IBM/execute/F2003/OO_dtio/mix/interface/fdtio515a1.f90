!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2005
!*
!*  DESCRIPTION                : DTIO generics (null values in the input record)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer :: i = -1
    end type

    interface read (formatted)
        subroutine formattedRead (dtv, unit, iotype, vlist, iostat, iomsg)
        import A
            class (A), intent(inout) :: dtv
            integer, intent(in) :: unit
            character (*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

subroutine formattedRead (dtv, unit, iotype, vlist, iostat, iomsg)
use m, only:A
    class (A), intent(inout) :: dtv
    integer, intent(in) :: unit
    character (*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    dtv%i = -1
    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%i
end subroutine


program fdtio515a1
use m
    type(a)  a1(3)

    write (1, *) '1, ,3, 4'
    write (1, *) ', 20,30,40'

    rewind (1)

    read (1, *) a1

    if (any (a1%i /= (/1, -1, 3/))) error stop 1_4

    read (1, *) a1

    if (any (a1%i /= (/-1, 20, 30/))) error stop 2_4

    close (1, status='delete')
end
