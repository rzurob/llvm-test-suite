! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 01/06/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (end of file condition on
!                               sequential access mode)
!                               adaptation: exposed length
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
        character(lbase_1), allocatable :: name

        contains

        procedure :: read => readBase
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

    contains

    subroutine readBase (b, unit, iotype, v_list, iostat, iomsg)
        class (base(*)), intent(inout) :: b ! tcx: (*)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (allocated (b%name)) deallocate (b%name)

        allocate (b%name)

    read (unit, *, iostat=iostat, iomsg=iomsg) b%name
    end subroutine
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    call dtv%read (unit, iotype, v_list, iostat, iomsg)
end subroutine


program fdtio514kl
use m
use iso_fortran_env
    integer stat1
    character(200) err

    class (base(:)), allocatable :: b1(:) ! tcx: (:)

    allocate (base(10)::b1(2)) ! tcx: base(10)

    open (1, file='fdtio514kl.data')

    write (1, '(a)') '     abcefg'

    rewind (1)

    !! This read should fail for b1(2)
    read (1, *, iostat=stat1, iomsg=err) b1(1), b1(2)

    if (stat1 /= iostat_end) then
        print *, stat1, err
        error stop 101_4
    end if

    if (.not. allocated (b1(1)%name)) error stop 2_4

    if (b1(1)%name /= 'abcefg') error stop 3_4

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (10) / declare with (*) - 4 changes