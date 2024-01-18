! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio513akl
!*
!*  DATE                       : 2007-08-16 (original: 01/04/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (use type-bound to do the IO
!                               during the DTIO; test on internal file)
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
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        integer(kbase_1), allocatable :: id(:)

        contains

        procedure :: print => printBase
    end type

    interface write(formatted)
        subroutine formattedWrite4Base (dtv, unit, iotype, vlist, iostat, iomsg)
        import base
            class (base(8)), intent(in) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine printBase (b, unit, stat, err)
        class (base(8)), intent(in) :: b ! tcx: (8)
        integer, intent(in) :: unit

        integer, intent(out) :: stat
        character(*), intent(inout) :: err

        if (allocated (b%id)) then
            write (unit, 100, iostat=stat, iomsg=err) lbound(b%id, 1), &
                                ubound(b%id, 1)

            if (stat /= 0) return

            write (unit, *, iostat=stat, iomsg=err) b%id

        end if

100 format ("bounds: ", 2i5, ";")
    end subroutine
end module

subroutine formattedWrite4Base (dtv, unit, iotype, vlist, iostat, iomsg)
use m, only: base
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    call dtv%print(unit, iostat, iomsg)
end subroutine


program fdtio513akl
use m
    class (base(8)), allocatable :: b1(:) ! tcx: (8)

    integer(8) i1(0:1), stat

    character (200) msg, err

    i1 = (/-10_8, -1_8/)

    allocate (b1(2), source=(/base(8)((/10_8, 1_8/)), base(8) (i1)/)) ! tcx: (8) ! tcx: (8)

    write (msg, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 101_4
    end if

    print *, msg
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 6 changes
