! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/24/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (test the value of iotype is
!                               "LISTDIRECTED" for list-directed parent data
!                               transfer invocation)
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
    type base (kb) ! kb=4
       integer, kind :: kb
        complex(kb), allocatable :: data
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only:base
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') error stop 10_4

    if (.not. allocated(dtv%data)) allocate (dtv%data)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio004kl
use m
    class (base(4)), pointer :: b1 ! tcx: (4)

    type (base(4)), target :: b11 ! tcx: (4)

    integer stat
    character(200) err

    logical precision_x8

    b1 => b11

    write (1, *) (1.0e0_4, 2.1e0_4)

    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    !! verify b11

    if (.not. allocated (b11%data)) error stop 2_4

    if (.not. precision_x8 (b11%data, (1.0e0_4, 2.1e0_4))) error stop 3_4

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 4 changes
