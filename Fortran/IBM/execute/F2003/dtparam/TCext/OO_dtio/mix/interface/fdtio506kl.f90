! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio506kl
!*
!*  DATE                       : 2007-08-14 (original: 03/03/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (DTIO calls during DTIO)
!                               adaptation: exposed kind
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: i => null()
    end type

    integer(4), parameter :: ISNULL = -999999

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), allocatable :: data ! tcx: (kcontainer_1)
    end type

    integer(4), parameter :: ISALLOCATED = 1
    integer(4), parameter :: UNALLOCATED = 0
end module

module m1
use m
    interface read(unformatted)
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedReadContainer (dtv, unit, iostat, iomsg)
        import container
            class (container(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedWriteContainer (dtv, unit, iostat, iomsg)
        import container
            class (container(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


program fdtio506kl
use m1
    integer stat
    character(200) :: errormsg

    integer(4), target :: i1 = 200

    type (container(4)) co1(3) ! tcx: (4)

    allocate (co1(2)%data)
    allocate (co1(2)%data%i, source=100)

    allocate (co1(3)%data, source=base(4)(null())) ! tcx: (4)

    open (1, file='fdtio506kl.data', form='unformatted')

    write (1, iostat=stat, iomsg=errormsg) co1

    if (stat /= 0) then
        print *, stat, errormsg
        error stop 101_4
    end if

    rewind 1

    read (1, iostat=stat, iomsg=errormsg) co1(3:1:-1)

    if (stat /= 0) then
        print *, stat, errormsg
        error stop 2_4
    end if

    if ((.not. allocated (co1(1)%data)) .or. (.not. allocated (co1(2)%data)) &
           .or. allocated (co1(3)%data)) error stop 3_4

    if (associated (co1(1)%data%i)) error stop 4_4

    if (.not. associated (co1(2)%data%i)) error stop 5_4

    if (co1(2)%data%i /= 100) error stop 6_4

    close(1, status='delete')
end


subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, err

    !! we do not care about the failure of deallocation
    if (associated (dtv%i))   deallocate (dtv%i, stat=err)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    !! check for NULL
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg) ISNULL
    end if
end subroutine


subroutine unformattedWriteContainer (dtv, unit, iostat, iomsg)
use m
use m1, only: write(unformatted)
    class (container(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! we use an extra field to test if the allocatable component is allocated
    if (.not. allocated (dtv%data)) then
        write (unit, iostat=iostat, iomsg=iomsg) unallocated, base(4)(null()) ! tcx: (4)
    else
        write (unit, iostat=iostat, iomsg=iomsg) isallocated, dtv%data
    end if
end subroutine


subroutine unformattedReadContainer (dtv, unit, iostat, iomsg)
use m
use m1, only: read(unformatted)
    class (container(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) flag

    type (base(4)) temp ! tcx: (4)

    if (allocated (dtv%data)) deallocate (dtv%data)

    !! read in the two field
    read (unit, iostat=iostat, iomsg=iomsg) flag, temp

    if (iostat /= 0) return

    if (flag == isallocated) then
        allocate (dtv%data, source=temp)
    end if
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 8 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 5 changes
