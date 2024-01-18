! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtioC936_001kl
!*
!*  DATE                       : 2007-07-23 (original: 11/4/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (basic functionality test for
!                               sequence type)
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
    type base (kb,lb) ! kb,lb=4,200
       integer, kind :: kb
       integer, len :: lb
        sequence

        integer(kb) :: id
        character(lb) :: name
    end type
end module

program fdtioC936_001kl
use m
    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            type (base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            type (base(4,*)), intent(in) :: dtv ! tcx: (4,*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base(4,200)) b1 ! tcx: (4,200)

    b1 = base(4,200)(100, 'xlftest 101') ! tcx: (4,200)

    write (1, *) b1

    rewind (1)

    read (1, *) b1

    if (b1%id /= 111) error stop 1_4
    if (b1%name /= 'xlftest 101') error stop 2_4

    close(1, status='delete')
end

subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    type (base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! use i6 for dtv%id to account for the leading blank character in list
    !directed write
    read (unit, '(i3, a20)', iostat = iostat, iomsg = iomsg) dtv%id, dtv%name

    dtv%id = dtv%id + 10
end subroutine

subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    type (base(4,*)), intent(in) :: dtv ! tcx: (4,*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(i5, a20)', iostat = iostat, iomsg = iomsg) dtv%id+1, dtv%name
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,200) / declare with (4,*) - 6 changes
