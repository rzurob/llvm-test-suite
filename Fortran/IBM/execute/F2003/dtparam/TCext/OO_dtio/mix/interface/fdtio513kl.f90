! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio513kl
!*
!*  DATE                       : 2007-08-16 (original: 01/04/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (DTIO used in the type-bound
!                               procedures)
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

    subroutine printBase (b, unit)
        class (base(8)), intent(in) :: b ! tcx: (8)
        integer, intent(in) :: unit

        integer stat
        character(200) err

        write (unit, *, iostat=stat, iomsg=err) b

        if (stat /= 0) then
            print *, stat, err

            error stop 10_4
        end if
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

    if (allocated (dtv%id)) then
        write (unit, 100, iostat=iostat, iomsg=iomsg) lbound(dtv%id, 1), &
                                ubound(dtv%id, 1)

        if (iostat /= 0) return

        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%id
    end if

100 format ("bounds: ", 2i5, ";")
end subroutine


program fdtio513kl
use m
    class (base(8)), allocatable :: b1(:) ! tcx: (8)

    integer(8) i1(0:1)

    i1 = (/-10_8, -1_8/)

    allocate (b1(2), source=(/base(8)((/10_8, 1_8/)), base(8) (i1)/)) ! tcx: (8) ! tcx: (8)

    open (10, file='fdtio513kl.data')

    call b1(1)%print(10)
    call b1(2)%print(10)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 6 changes
