! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/16/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (test unit value; for internal
!                               file the unit value is negative values)
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
    type base (lb) ! lb=5
       integer, len :: lb
        character(lb), allocatable :: data
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

subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(len(dtv%data)) temp

    !! test that unit is negative value

    if (unit >= 0) then
        iostat=unit
        iomsg = 'negative value is expected'
        return
    end if

    read (unit, *, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=temp)
end subroutine

program fdtio002kl
use m
    class (base(:)), pointer :: b1 ! tcx: (:)

    integer stat
    character(200) err

    character(10) :: c1 = 'abcdefghij'

    allocate (base(5)::b1) ! tcx: base(5)

    read(c1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    !! verify the read in data
    if (.not. allocated (b1%data)) error stop 2_4

    if (b1%data /= 'abcde') error stop 3_4
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (5) / declare with (*) - 3 changes
