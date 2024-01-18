! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio520a1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio520a1 by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 03/08/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (list-directed read for a
!                               linked-list; source is multi-line input)
!                               adaptation: exposed kind
!*
!*
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
    !! also multi-line input
    recursive subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
    use iso_fortran_env
        class (node(8)), intent(inout) :: dtv ! tcx: (8)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(15) c1

        !! we only handle the DT edit descriptor
        if (iotype(1:2) /= 'DT') return

        if (size(v_list) /= 0) error stop 10_4

        read (unit, '(a15)', iostat=iostat, iomsg=iomsg) c1

        if (iostat == iostat_eor) then  !! end of line encoutered
            iostat = 0

            !! move to next line
            read (unit, '(/,a)', iostat=iostat, iomsg=iomsg) c1(1:0)

            if (iostat /= 0) error stop 13_4
        else

            if (iostat /= 0) error stop 11_4

            read (c1, '(g15.2)', iostat=iostat, iomsg=iomsg) dtv%value

            if (iostat /= 0) error stop 12_4
        end if

        !! peek next character
        read (unit, '(a1,TL1)', iostat=iostat, iomsg=iomsg) c1(1:1)

        if (iostat == iostat_eor) then
            read (unit, '(/, a1,TL1)', iostat=iostat, iomsg=iomsg) c1(1:1)

            if (iostat /= 0) error stop 14_4
        end if

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


program fdtio520a1kl
use m
    type (node(8)) :: list ! tcx: (8)

    integer stat1
    character(200) err

    write (1, '(3g15.2)') (j*1.5, j = 1, 3)

    write (1, '(2g15.2)') (j*3.0, j = 4, 5)
    write (1, '(a1)') '/'

    rewind (1)

    read (1, '(DT)', iostat=stat1, iomsg=err) list

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    call list%print ("(f10.2)")

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: node - added parameters (knode_1) to invoke with (8) / declare with (8) - 5 changes
