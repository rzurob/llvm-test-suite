! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 03/03/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)

!*
!*  DESCRIPTION                :
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (lbase_1) ! lbase_1=150
       integer, len :: lbase_1
        character(lbase_1), allocatable :: name
    end type
end module

program fdtio507a6kl
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base(*)), intent(inout) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base(*)), intent(in) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class (base(:)), allocatable :: b1(:) ! tcx: (:)
    integer stat
    character(200) err

    err = 'no error'
    allocate (base(150)::b1(2)) ! tcx: base(150)

    open (1, file='fdtio507a6kl.data', form='unformatted')

    write (1, iostat=stat, iomsg=err) base(150)('xlftest'), base(150)(null()) ! tcx: (150) ! tcx: (150)

    if (stat /= 0) error stop 3_4

    backspace (1)

    read (1) b1(1), b1(2)       !<-- this should fail

    print *, stat, err
    print *, b1(1)%name, b1(2)%name
    error stop 10_4

100 rewind (1)


    close (1, status='delete')
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(150) name

    integer stat1

    read (unit, iostat=iostat, iomsg=iomsg) name

    if (iostat /= 0) return

    deallocate (dtv%name, stat=stat1)
    allocate (dtv%name, source=name)

end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%name)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%name
    else
        write (unit, iostat=iostat, iomsg=iomsg) 'U'
    end if
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (150) / declare with (*) - 7 changes