! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio521kl
!*
!*  DATE                       : 2007-08-16 (original: 05/31/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (dummy procedure used as the DTIO
!                               routine)
!                               adaptation: exposed kind
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        integer(kbase_1), allocatable :: data(:)
    end type


    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(8)), intent(in) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


module n
use m, only: base

    contains

    subroutine printBase (b, dtioRoutine)
        class(base(8)), intent(in) :: b(:) ! tcx: (8)

        interface write(formatted)
            subroutine dtioRoutine (dtv, unit, iotype, v_list, iostat, iomsg)
            import base
                class (base(8)), intent(in) :: dtv ! tcx: (8)
                integer, intent(in) :: unit
                character(*), intent(in) :: iotype
                integer, intent(in) :: v_list(:)
                integer, intent(out) :: iostat
                character(*), intent(inout) :: iomsg
            end subroutine
        end interface

        print *, b(1)  !<-- how b is printed depends on the dummy-proc dtioRoutine
        print *, b  !<-- how b is printed depends on the dummy-proc dtioRoutine
    end subroutine
end module


program fdtio521kl
use m
use n
    procedure (formattedWrite) p1, p2

    type (base(8)) :: b1(2) ! tcx: (8)

    allocate (b1(1)%data(2), source=(/1_8, 10_8/))
    allocate (b1(2)%data(2), source=(/-10_8, -1_8/))

    call printBase(b1, p1)

    call printBase(b1, p2)
end


!! list-directed write during child data transfer
subroutine p1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
    end if
end subroutine


!! p2 is a reverse print of dtv%data
subroutine p2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 15_4

    if (allocated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) &
            dtv%data(ubound(dtv%data,1):lbound(dtv%data,1):-1)
    end if
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 6 changes
