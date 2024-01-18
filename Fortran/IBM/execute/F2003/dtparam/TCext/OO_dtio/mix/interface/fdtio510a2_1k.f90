! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio510a2_1k
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio510a2_1 by Jim Xia)
!*  DATE                       : 2007-08-15 (original: 11/17/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (one DTIO subroutine calls
!                               different subroutines based on the dynamic type
!                               of dtv)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), allocatable :: i1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        real(kbase_1), allocatable :: r1

        contains

        procedure :: print => printChild
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
        type (child(4)), intent(inout) :: dtv ! tcx: (4)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated (dtv%i1)) allocate (dtv%i1)
        if (.not. allocated (dtv%r1)) allocate (dtv%r1)

        read (unit, iostat=iostat, iomsg=iomsg) dtv%i1, dtv%r1
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%i1
    end subroutine

    subroutine printChild (b)
        class (child(4)), intent(in) :: b ! tcx: (4)

        print '(i5,f10.2)', b%i1, b%r1
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
        type is (child(4)) ! tcx: (4)
            call readUChild (dtv, unit, iostat, iomsg)
        class default
            error stop 10_4
    end select
end subroutine

program fdtio510a2_1k
use m
    class (base(4)), allocatable :: b1, b2 ! tcx: (4)

    integer stat
    character(200) err

    allocate (b1)
    allocate (child(4):: b2) ! tcx: (4)

    write (1) 100, 200, 2.1

    rewind 1

    read (1, iostat=stat, iomsg=err) b1, b2

    if (stat /= 0) then
        print *, stat, err
        error stop 101_4
    end if

    call b1%print
    call b2%print
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 4 changes
