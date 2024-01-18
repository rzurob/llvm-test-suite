! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio510kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio510 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 03/08/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (recursive DTIO routines)
!                               adaptation: exposed kind
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node (knode_1) ! knode_1=4
       integer, kind :: knode_1
        type (node(knode_1)), pointer :: next => null()
        integer(knode_1) :: data

        contains

        final :: deallocateAllNodes
    end type

    integer(4), parameter :: ENDLIST = -99999

    interface read(unformatted)
        recursive subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import node, ENDLIST
            class (node(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        recursive subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import node, ENDLIST
            class (node(4)), intent(in) :: dtv ! tcx: (4)
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

program fdtio510kl
use m
    type (node(4)) :: n1, n2 ! tcx: (4)

    type (node(4)), pointer :: iterator ! tcx: (4)
    integer stat1
    character(200) err


    n1%data = 1

    allocate (n1%next, source=node(4)(data=10)) ! tcx: (4)
    allocate (n1%next%next, source=node(4) (data=100)) ! tcx: (4)
    allocate (n1%next%next%next, source=node(4)(data=1000)) ! tcx: (4)

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
    class (node(4)), intent(inout) :: dtv ! tcx: (4)
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
    class (node(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    type (node(4)), pointer :: iterator ! tcx: (4)

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


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode_1) to invoke with (4) / declare with (4) - 11 changes
