! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 01/04/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (a test case that use type-bounds
!                               for DTIO interface)
!                               adaptation: kind, length exposed at abstract base
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
    type, abstract :: base (kbase,lbase) ! kbase,lbase=4,20
       integer, kind :: kbase
       integer, len :: lbase
        contains

        procedure(printBase), deferred :: print
    end type

    interface
        subroutine printBase (b, unit, iotype, vlist, iostat, iomsg)
        import base
            class (base(4,*)), intent(in) :: b ! tcx: (4,*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

module m1
use m
    type, extends(base) :: child
        integer(kbase), allocatable :: id

        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3
        character(lbase) :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    !! assume iotype is 'LISTDIRECTED'
    subroutine printChild (b, unit, iotype, vlist, iostat, iomsg)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*,)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') error stop 10_4

        if (allocated (b%id)) then
            write (unit, *, iostat=iostat, iomsg=iomsg) b%id
        end if
    end subroutine

    subroutine printGen3 (b, unit, iotype, vlist, iostat, iomsg)
        class (gen3(4,*)), intent(in) :: b ! tcx: (4,*,,)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') error stop 20_4

        if (allocated (b%id)) then
            write (unit, '(i8,2a)', iostat=iostat, iomsg=iomsg) b%id, '; ', b%name
        else
            write (unit, *, iostat=iostat, iomsg=iomsg) b%name
        end if
    end subroutine
end module


module n
    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (base(4,*)), intent(in) :: dtv ! tcx: (4,*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (base(4,*)), intent(in) :: dtv ! tcx: (4,*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    call dtv%print (unit, iotype, v_list, iostat, iomsg)
end subroutine


program fdtio513a1klAbs
use m1
use n
    class (base(4,:)), allocatable :: b1(:) ! tcx: (4,:)

    allocate (b1(0:1), source=(/gen3(4,20)(1, 'xlftest'), gen3(4,20)(null(), 'team')/)) ! tcx: (4,20,,) ! tcx: (4,20,,)

    print *, b1
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase,lbase) to invoke with (4,20) / declare with (4,*) - 4 changes
! type: child - added parameters () to invoke with (4,20,) / declare with (4,*,) - 1 changes
! type: gen3 - added parameters () to invoke with (4,20,,) / declare with (4,*,,) - 3 changes