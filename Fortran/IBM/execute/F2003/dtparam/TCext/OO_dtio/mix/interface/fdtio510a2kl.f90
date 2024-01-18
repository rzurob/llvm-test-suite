! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio510a2kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio510a2 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 03/07/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)

!*  DRIVER STANZA              : xlf2003

!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (subroutine calls during the DTIO
!                               procedure)
!                               adaptation: exposed kinds
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), allocatable :: i1
    end type

    type, extends(base) :: child (kchild_1) ! kchild_1=4
       integer, kind :: kchild_1
        real(kchild_1), allocatable :: r1
    end type

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine readUBase (dtv, unit, iostat, iomsg)
        type (base(4)), intent(inout) :: dtv ! tcx: (4)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated (dtv%i1)) allocate (dtv%i1)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%i1
    end subroutine

    subroutine readUChild (dtv, unit, iostat, iomsg)
        type (child(4,4)), intent(inout) :: dtv ! tcx: (4,4)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated (dtv%i1)) allocate (dtv%i1)
        if (.not. allocated (dtv%r1)) allocate (dtv%r1)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%i1, dtv%r1
    end subroutine
end module


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only: base, child, readUBase, readUChild
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
        type is (base(4)) ! tcx: (4)
            call readUBase (dtv, unit, iostat, iomsg)
        type is (child(4,4)) ! tcx: (4,4)
            call readUChild (dtv, unit, iostat, iomsg)
        class default
            error stop 20_4
    end select
end subroutine

program fdtio510a2kl
use m
    class (base(4)), allocatable :: b1, b2 ! tcx: (4)

    integer stat
    character(200) err

    logical(4) precision_r4

    allocate (b1)
    allocate (child(4,4):: b2) ! tcx: (4,4)

    write (1) 200, 2.1
    write (1) 300

    rewind 1

    read (1, iostat=stat, iomsg=err) b2

    if (stat /= 0) then
        print *, stat, err
        error stop 2_4
    end if

    select type (b2)
        type is (child(4,4)) ! tcx: (4,4)
            if (b2%i1/= 200) error stop 3_4

            if (.not. precision_r4 (b2%r1, 2.1_4)) error stop 4_4
        class default
            error stop 5_4
    end select

    read (1, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 6_4
    end if

    if (b1%i1 /= 300) error stop 7_4

    close(1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters (kchild_1) to invoke with (4,4) / declare with (4,4) - 4 changes
