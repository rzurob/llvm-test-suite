! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2005
!*
!*  DESCRIPTION                : DTIO generics (IOSTAT should be passed back to
!                               the caller even the failure happens during the
!                               multi-level DTIO calls)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node
        type (node), pointer :: next => null()
        integer(4) :: data

        contains

        final :: deallocateAllNodes
    end type

    interface read(unformatted)
        recursive subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import node
            class (node), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    recursive subroutine deallocateAllNodes (n)
        type(node), intent(inout) :: n

        if (associated (n%next)) deallocate (n%next)
    end subroutine
end module

program fdtio510_1
use m
    type (node), target :: n1

    type (node), pointer :: iterator
    integer stat1
    character(200) err


    write (1) 1, 10, 100, 1000

    rewind (1)

    read (1, iostat=stat1, iomsg=err) n1

    if ((stat1 /= 100) .or. (err /= 'list ended')) then
        print *, stat1, err
        error stop 3_4
    end if

    iterator => n1

    do while (associated (iterator))
        print *, iterator%data
        iterator => iterator%next
    end do
end


recursive subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: node, read(unformatted)
    class (node), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg


    if (associated (dtv%next)) deallocate (dtv%next)

    read (unit, iostat=iostat, iomsg=iomsg) dtv%data

    if (iostat /= 0) return

    allocate (dtv%next)

    read (unit, iostat=iostat, iomsg=iomsg) dtv%next

    if (iostat == 3) then
        deallocate (dtv%next)
        iostat = 100
        iomsg = 'list ended'
    end if
end subroutine
