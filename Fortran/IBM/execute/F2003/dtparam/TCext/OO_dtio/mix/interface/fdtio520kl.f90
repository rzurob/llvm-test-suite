! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio520kl
!*
!*  DATE                       : 2007-08-16 (original: 01/17/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (list-directed write for a
!                               linked-list; basic test)
!                               adaptation: exposed kind
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
    type node (knode_1) ! knode_1=8
       integer, kind :: knode_1
        integer(knode_1) :: value
        type (node(knode_1)), pointer :: next => null() ! tcx: (8)
    end type


    interface write(formatted)
        module procedure formattedWrite
    end interface

    contains

    recursive subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (node(8)), intent(in) :: dtv ! tcx: (8)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        write (unit, '(i10,/)', iostat=iostat, iomsg=iomsg) dtv%value

        if (iostat /= 0) return

        if (associated (dtv%next)) write (unit, *, iostat=iostat, iomsg=iomsg) &
                        dtv%next
    end subroutine
end module


program fdtio520kl
use m
    type (node(8)) :: list ! tcx: (8)

    integer stat1
    character(200) err

    list%value=1_8

    !! add 2 nodes
    allocate(list%next)
    allocate(list%next%next)

    list%next%value=2
    list%next%next%value=3

    write (*, *, iostat=stat1, iomsg=err) list

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 101_4
    end if
end


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode_1) to invoke with (8) / declare with (8) - 3 changes
