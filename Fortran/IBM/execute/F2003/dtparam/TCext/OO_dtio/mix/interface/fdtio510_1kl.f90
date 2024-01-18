! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio510_1kl
!*
!*  DATE                       : 2007-08-15 (original: 04/27/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (IOSTAT should be passed back to
!                               the caller even the failure happens during the
!                               multi-level DTIO calls)
!                               adaptation: exposed kind
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node (knode_1) ! knode_1=4
       integer, kind :: knode_1
        type (node(knode_1)), pointer :: next => null() ! tcx: (knode_1)
        integer(knode_1) :: data

        contains

        final :: deallocateAllNodes
    end type

    interface read(unformatted)
        recursive subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import node
            class (node(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    recursive subroutine deallocateAllNodes (n)
        type(node(4)), intent(inout) :: n ! tcx: (4)

        if (associated (n%next)) deallocate (n%next)
    end subroutine
end module

program fdtio510_1kl
use m
    type (node(4)), target :: n1 ! tcx: (4)

    type (node(4)), pointer :: iterator ! tcx: (4)
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
    class (node(4)), intent(inout) :: dtv ! tcx: (4)
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


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode_1) to invoke with (4) / declare with (4) - 6 changes
