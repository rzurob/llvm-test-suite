! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio514a2kl
!*
!*  DATE                       : 2007-08-16 (original: 01/10/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (EOF condition for stream file)
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
        integer(kbase_1), allocatable :: data(:)
    end type

    interface read (formatted)
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
use m, only: base
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') then
        error stop 10_4
    end if

    if (size (v_list) /= 0) then
        error stop 11_4
    end if

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data(3))

    !! we blindly read in 3 data
    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine

program fdtio514a2kl
use m
use iso_fortran_env
    class (base(4)), allocatable :: b1(:) ! tcx: (4)

    integer stat1
    character(200) err

    open (1, file='fdtio514a2kl.data', access='stream', form='formatted')

    write (1, pos=1, fmt='(5i2)') 1, 2, 3, 4, 5

    allocate (b1(2))

    read (1, pos=1, fmt=*, iostat=stat1, iomsg=err) b1(1), b1(2)

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 101_4
    end if

    !! the first element of b1 is read in correctly; verify that
    if (.not. allocated (b1(1)%data)) error stop 2_4

    if (any(b1(1)%data /= (/1, 2, 3/))) error stop 3_4

    !! 2nd test use different position

    read (1, *, pos=7, iostat=stat1, iomsg=err) b1(2)

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 4_4
    end if

    close (1, status = 'delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
