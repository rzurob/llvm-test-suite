! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio519akl
!*
!*  DATE                       : 2007-08-16 (original: 01/17/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (list-directed parent write for
!                               derived type with private component)
!                               adaptation: exposed kind, len
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
    type base (kbase_1,lbase_1) ! kbase_1,lbase_1=4,2
       integer, kind :: kbase_1
       integer, len :: lbase_1
        integer(kbase_1), private :: data (lbase_1)

        contains

        procedure :: read => readBase
        procedure :: getVal => getBaseVal
    end type


    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine readBase (b, unit, iostat, iomsg)
        class (base(4,*)), intent (inout) :: b ! tcx: (4,*)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        read (unit, *, iostat=iostat, iomsg=iomsg) b%data
    end subroutine

    integer function getBaseVal (b)
        class (base(4,*)), intent(in) :: b ! tcx: (4,*)
        dimension getBaseVal (2)

        getBaseVal = b%data
    end function
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only : base
    class (base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    call dtv%read (unit, iostat=iostat, iomsg=iomsg)
end subroutine


program fdtio519akl
use m
    class (base(4,:)), allocatable :: b1(:,:) ! tcx: (4,:)

    integer stat1
    character(200) err

    allocate (base(4,2)::b1(2,1)) ! tcx: base(4,2)

    write (1, '(5i10)') 10, 3, 1, 5, 7
    write (1, '(4i10)') 2, 3, 5, 6

    rewind 1

    !! test read for scalar
    read (1, *, iostat=stat1, iomsg=err) b1(1,1)

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 101_4
    end if

    if (any(b1(1,1)%getVal() /= (/10, 3/))) error stop 2_4


    read (1, *, iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 3_4
    end if

    if (any(b1(1,1)%getVal() /= (/2, 3/))) error stop 4_4
    if (any(b1(2,1)%getVal() /= (/5, 6/))) error stop 5_4

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2) to invoke with (2,4) / declare with (2,4) - 5 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (4,2) / declare with (4,*) - 5 changes
