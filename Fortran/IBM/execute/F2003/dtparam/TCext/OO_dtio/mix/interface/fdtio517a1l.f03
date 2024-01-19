! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 01/18/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (non-advancing DTIO on internal
!                               file should fail)
!                               adaptation: exposed len
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
    type base (lbase_1) ! lbase_1=10
       integer, len :: lbase_1
        character (lbase_1), allocatable :: data(:)
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(*)), intent(inout) :: dtv ! tcx: (*)
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
    class (base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') error stop 10_4

    if (size (v_list) /= 0) error stop 11_4

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data(2)) ! tcx: base(10)

    read (unit, '(2a10)', advance='no', iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio517a1l
use m
use iso_fortran_env
    class (base(:)), allocatable :: b1(:) ! tcx: (:)

    integer stat1
    character(200) err
    logical precision_r8

    character (30) c1

    allocate (base(10)::b1(3)) ! tcx: base(10)

    write (c1, '(a10,a)') '0123456789', new_line('a')
    write (c1(12:), '(a10, a8)') '9876543', 'abcdef'

    !! this read statement will fail as DTIO fails due to EOF
    read (c1(12:), *, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= IOSTAT_EOR) then
        print *, stat1, err
        error stop 101_4
    end if

    !! similarly the read for array b1 will fail
    read (c1, *, iostat=stat1, iomsg=err) b1

    if (stat1 /= IOSTAT_EOR) then
        print *, stat1, err
        error stop 2_4
    end if

end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,lbase_1) to invoke with (1,10) / declare with (1,*) - 3 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (1,10) / declare with (1,*) - 3 changes
! type: base - added parameters (lbase_1) to invoke with (10) / declare with (*) - 3 changes
