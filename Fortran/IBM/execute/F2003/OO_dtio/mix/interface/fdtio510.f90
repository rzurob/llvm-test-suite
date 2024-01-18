! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/08/2005
!*
!*  DESCRIPTION                : DTIO generics (recursive DTIO routines)
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

    integer(4), parameter :: ENDLIST = -99999

    interface read(unformatted)
        recursive subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import node, ENDLIST
            class (node), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        recursive subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import node, ENDLIST
            class (node), intent(in) :: dtv
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

program fdtio510
use m
    type (node) :: n1, n2

    type (node), pointer :: iterator
    integer stat1
    character(200) err


    n1%data = 1

    allocate (n1%next, source=node(data=10))
    allocate (n1%next%next, source=node (data=100))
    allocate (n1%next%next%next, source=node(data=1000))

    write (1, iostat=stat1, iomsg=err) n1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    rewind (1)

    read (1, iostat=stat1, iomsg=err) n2

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 3_4
    end if

    !! verify n2
    if (n2%data /= 1) error stop 4_4

    iterator => n2%next

    if (.not. associated (iterator)) error stop 5_4
    if (iterator%data /= 10) error stop 6_4

    iterator => iterator%next
    if (.not. associated (iterator)) error stop 7_4
    if (iterator%data /= 100) error stop 8_4

    iterator => iterator%next
    if (.not. associated (iterator)) error stop 9_4
    if (iterator%data /= 1000) error stop 10_4

    iterator => iterator%next
    if (associated (iterator)) error stop 11_4

    close (1, status='delete')
end


recursive subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: node, read(unformatted), ENDLIST
    class (node), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg


    if (associated (dtv%next)) deallocate (dtv%next)

    read (unit, iostat=iostat, iomsg=iomsg) dtv%data

    if (iostat /= 0) error stop 15_4

    if (dtv%data /= ENDLIST) then

        allocate (dtv%next)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%next

        if (iostat /= 0) error stop 16_4

        if (dtv%next%data == ENDLIST) then
            deallocate (dtv%next)
        end if
    end if

end subroutine

recursive subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m, only: node, write(unformatted), ENDLIST
    class (node), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    type (node), pointer :: iterator

    nullify (iterator)

    write (unit, iostat=iostat, iomsg=iomsg)  dtv%data

    if (iostat /= 0) error stop 20_4

    if (associated (dtv%next)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%next
    else
        write (unit, iostat=iostat, iomsg=iomsg) ENDLIST
    end if

    if (iostat /= 0) error stop 25_4
end subroutine
