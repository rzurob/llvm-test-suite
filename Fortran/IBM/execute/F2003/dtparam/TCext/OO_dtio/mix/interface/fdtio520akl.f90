! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 03/08/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (list-directed read operation for
!                               a linked-list)
!                               adaptation: exposed kind
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node (knode_1) ! knode_1=8
       integer, kind :: knode_1
        real(knode_1) value
        type (node(knode_1)), pointer :: next => null() ! tcx: (knode_1)

        contains

        procedure :: print => formattedPrintNode
    end type

    interface read(formatted)
        module procedure formattedRead
    end interface

    contains

    !! assume there is at least one node's data for read
    recursive subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        class (node(8)), intent(inout) :: dtv ! tcx: (8)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(15) c1

        !! we only handles the DT edit descript
        if (iotype(1:2) /= 'DT') return

        if (size(v_list) /= 0) error stop 10_4

        read (unit, '(a15)', iostat=iostat, iomsg=iomsg) c1

        if (iostat /= 0) return

        read (c1, '(g15.2)', iostat=iostat, iomsg=iomsg) dtv%value

        if (iostat /= 0) return

        !! peek next character
        read (unit, '(a1,TL1)', iostat=iostat, iomsg=iomsg) c1(1:1)

        !! '/' is a marker tells the program to terminate the read
        if (c1(1:1) == '/') return

        allocate (dtv%next)

        !! keep reading
        read (unit, '(DT)', iostat=iostat, iomsg=iomsg) dtv%next
    end subroutine

    subroutine formattedPrintNode (n, fmt)
        class (node(8)), intent(in) :: n ! tcx: (8)
        character(*), intent(in) :: fmt

        type (node(8)), pointer :: iterator ! tcx: (8)

        write (*, fmt) n%value

        iterator => n%next

        do while (associated (iterator))
            write (*, fmt) iterator%value

            iterator => iterator%next
        end do
    end subroutine
end module


program fdtio520akl
use m
    type (node(8)) :: list ! tcx: (8)

    integer stat1
    character(200) err

    write (1, '(5g15.2, a1)') (j*1.5, j = 1, 5), '/'

    rewind (1)

    read (1, '(DT)') list

    call list%print ("(f10.2)")

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode_1) to invoke with (8) / declare with (8) - 5 changes
