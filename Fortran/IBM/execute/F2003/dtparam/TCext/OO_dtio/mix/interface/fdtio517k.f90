! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 01/11/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (EOR condition for sequential
!                               formatted read)
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
        real (kbase_1), allocatable :: data(:)
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(8)), intent(inout) :: dtv ! tcx: (8)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


!! this subroutine only treats the list-directed read
subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(8)), intent(inout) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') error stop 10_4

    if (size (v_list) /= 0) error stop 11_4

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data(2))

    !! child data transfer is always non-advancing
    read (unit, '(2g8.1)', iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio517k
use m
use iso_fortran_env
    class (base(8)), allocatable :: b1(:) ! tcx: (8)

    integer stat1
    character(200) err
    logical precision_r8

    allocate (b1(2))

    write (1, '(g8.2)') 10.2
    write (1, '(2g8.2, "    ")') 1.2, 3.2

    rewind 1

    !! the 1st read will result in EOR condition; one line with only 1 data
    read (1, *, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= iostat_eor) then
        print *, stat1, err
        error stop 101_4
    end if


    !! the 2nd read will succeed
    read (1, *, iostat=stat1, iomsg=err) b1(2)

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    !! verify b1(2)

    if (.not. allocated (b1(2)%data)) error stop 3_4

    if (.not. precision_r8(b1(2)%data(1), 1.2e0_8)) error stop 4_4
    if (.not. precision_r8(b1(2)%data(2), 3.2e0_8)) error stop 5_4

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 3 changes
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 3 changes
