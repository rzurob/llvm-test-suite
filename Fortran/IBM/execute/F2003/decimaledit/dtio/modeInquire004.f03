! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/25/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test during child unformatted write statement an
!                               INQUIRE statement for decimal= on the unit
!                               returns value of UNDEFINED.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node
        character(:), allocatable :: name
        type(node), pointer :: next => null()

        contains

        procedure :: writeNodeUFmtd
        generic :: write(unformatted) => writeNodeUFmtd

        final :: finalizeNode
    end type

    type linkedList
        type(node), pointer :: head => null()

        contains

        procedure :: writeLListUFmtd
        generic :: write(unformatted) => writeLListUFmtd

        final :: finalizeLList
    end type

    contains

    recursive subroutine finalizeNode (n)
        type(node), intent(inout) :: n

        if (associated(n%next)) deallocate(n%next)
    end subroutine


    subroutine finalizeLList (ll)
        type(linkedList), intent(inout) :: ll

        if (associated(ll%head)) deallocate(ll%head)
    end subroutine


    recursive subroutine writeNodeUFmtd (dtv, unit, iostat, iomsg)
        class(node), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(:), allocatable, save :: localTemp

        localTemp = repeat(' ', 9)

        inquire(unit, decimal=localTemp, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        if (allocated(dtv%name)) then
            write (unit, iostat=iostat, iomsg=iomsg) dtv%name
        end if

        if (iostat /= 0) return

        write (unit, iostat=iostat, iomsg=iomsg) localTemp

        if (iostat /= 0) return

        if (associated(dtv%next)) then
            write (unit, iostat=iostat, iomsg=iomsg) dtv%next
        end if
    end subroutine

    subroutine writeLListUFmtd (dtv, unit, iostat, iomsg)
        class(linkedList), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (associated(dtv%head)) &
            write(unit, iostat=iostat, iomsg=iomsg) dtv%head
    end subroutine
end module

program modeInquire004
use m
    type(linkedList) :: list

    allocate(list%head)
    list%head = node('comma')

    allocate(list%head%next)
    list%head%next = node('point')

    allocate(list%head%next%next)
    list%head%next%next = node('unknown')

    open (1, access="stream")

    write (1) list
end